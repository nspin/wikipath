# Nick Spinale
# 10-4-2014

# this is a minimalistic script that,
# given two wikipedia articles as arguments,
# finds the shortest path of internal hyperlinks between them
# using iterated deepening.

import sys, urllib2
from bs4 import BeautifulSoup

start = sys.argv[1] # start article
end = sys.argv[2] # target article

# widipedia's articles' urls are all head + middle + article name
head, middle = 'https://en.wikipedia.org', '/wiki/'

# at any given point in the search, path will hold the trail taken
# to the current page
path = []

# during the searching towards a target depth, contains the pages
# that have already been searched
history = []

# given an article name, this returns all legal next articles.
# (normal wikipedia articles that are linked to from within the
# main body of the given article)
def getBranches(page):

    # recall the structure of articles' urls
    request = urllib2.Request(head + middle + page)
    response = urllib2.urlopen(request)
    html = response.read()
    soup = BeautifulSoup(html)

    # here we narrow the soup down to hyperlinks within the article's body
    links = soup.find(id='mw-content-text').find_all('a',href=True)
    # extract hrefs
    hrefs = [ link['href'] for link in links ]

    # filter those that do not link to normal articles (this filter seems
    # to work), drop the '/wiki/', and nub.
    return [ href[6:]
             for href in hrefs
             if middle in href and ':' not in href and '.org' not in href
           ]

# an iterative deepening search that, given a page, the distance from
# the current target depth, and the trail so far, checks for solution
# and either ends or backtracks if this distance is zero, otherwise
# calls itself for each possible branch.
# it also prints information about the search as it goes.
def search(page, distance):

    global history

    # print current page at correct depth
    offsetPrint(len(path), page)

    path.append(page)
    history.append(page)

    # if we're not at our target depth, then deepen
    if distance:
        for branch in getBranches(page):
            if branch not in history:
                search(branch, distance - 1)

    # if we're at our target depth and we have a match, end
    elif page == end:
        print '\nSOLUTION:'
        # prettyprint the path
        printPath(0, path)
        exit(0)

    # pick up breadcrumb
    path.pop()

# prettyprints the path (so that it looks like the progress printing above)
# increases indent with depth of step
def printPath(depth, remainingPath):
    if remainingPath:
        offsetPrint(depth, remainingPath[0])
        printPath(depth + 1, remainingPath[1:])


# prints a string at specified offset
def offsetPrint(offset, value):
    print '  ' * offset, '>', value


if __name__ == '__main__':
    depth = 0
    # repeatedly deepen search
    while True:
        # when restarting at a new depth, say so and clear history
        print '\nTARGET DEPTH:', depth
        history = []
        search(start, depth)
        depth += 1
