{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.QSem
import           Control.Lens hiding (elements)
import           Control.Monad
import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Lens
import           Data.Foldable hiding (elem)
import           Data.List
import           Data.Text.Lens
import           Data.Maybe
import qualified Data.Map as M
import           Data.Monoid
import           Network.Wreq
import           System.Environment
import           Text.Taggy.Lens

main = do
    -- Command line arguments. No checking here, just go.
    [start, end] <- getArgs
    -- Map from visited links to how we got there
    hist <- atomically $ newTVar M.empty
    -- To many requests at once seems to bother Wikipedia, theoretically, this could be as
    -- large as your machine could handle.
    sem <- newQSem 4
    let -- Given an article, get its forward neighbors.
        kids :: String -> IO [String]
        kids page = do
            waitQSem sem
            resp <- get ("http://www.wikipedia.com/wiki/" ++ page)
            signalQSem sem
            return $ links resp
        -- Central loop of the program
        go :: String -> String -> IO ()
        go prev curr = join . atomically $ do
            h <- readTVar hist
            if M.member curr h
             -- We've already been here
             then return sleep
             -- This is new terretory
             else let new = M.insert curr prev h
                  in do writeTVar hist new
                        return $ if curr == end
                                  -- Finish and print.
                                  then putStrLn . intercalate " --> " $ gather start end new
                                  -- Get children and recurse.
                                  else kids curr >>= choices (go curr)
    go undefined start

-- Synonym for readability
sleep :: IO ()
sleep = forever $ threadDelay maxBound

-- THIS IS SO LENS-LIKE
choices :: (a -> IO b) -> [a] -> IO b
choices f = runConcurrently . getAlt . foldMap (Alt . Concurrently . f)

-- Given a starting point and destination, backtrack through the map, returning the path.
gather :: String -> String -> M.Map String String -> [String]
gather start end map = (start :)
                     . reverse
                     . takeWhile (/= start)
                     $ iterate (map M.!) end


-- Extract links from a response. I love lenses, don't you?
links :: Response B.ByteString -> [String]
links = map (takeWhile (/= '#'))
      . filter (not . elem ':')
      . catMaybes
      . map (stripPrefix "/wiki/")
      . toListOf ( responseBody
                 . unpackedChars
                 . packed
                 . html
                 . elements
                 . allNamed (only "a")
                 . attr "href"
                 . _Just
                 . unpacked
                 )
