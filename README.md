wikipath
========

The Wikipedia game: given two articles, find the shortest path of internal hyperlinks between them.

This project currently contains two implementations of the program that plays this game.

### Python

The Python version, found in `./python`, depends on Beautiful Soup 4 (tested with 4.4.0), and has only been tested with python 2.7.
The easiest way to run it is with the [nix](https://nixos.org) package manager.
Inside of `./python/`, run `nix-shell` to enter a shell with everything the script needs, and then run:

```
python wikipedia.py [START] [END]
```

### Haskell

The Haskell version is found in `./haskell`.
It has more dependencies that the Python version (because Haskell is so composable, and because GHC comes with fewer additional libraries than CPython), but it includes a cabal file.
`cabal build` should be sufficient, but [nix](https://nixos.org) is certainly a safer bet (depending on the state of your `~/.cabal` directory).
Simply run `nix-build`, and you will find the binary in `./result/bin/`.
Usage is the same as with the Python version:

```
wikipedia [START] [END]
```

*Note:* These programs put strain on Wikipedia's servers, so use them with descretion.
