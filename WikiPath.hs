{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
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

-- Why doesn't this already exist (for all alternatives)
instance Monoid (Concurrently a) where
    mempty = empty
    mappend = (<|>)

sleep = forever $ threadDelay maxBound

main = do
    [start, end] <- getArgs
    hist <- atomically $ newTVar M.empty
    let go prev curr = join . atomically $ do
            h <- readTVar hist
            if M.member curr h
            then return sleep
            else let new = M.insert curr prev h
                 in do writeTVar hist new
                       return $ if curr == end
                                then print . intercalate " --> " $ gather start end new
                                else kids curr >>= choices (go curr)
    go undefined start

-- THIS IS SO LENS-LIKE
choices :: (a -> IO b) -> [a] -> IO b
choices f = runConcurrently . foldMap (Concurrently . f)

gather :: String -> String -> M.Map String String -> [String]
gather start end map = (start :)
                     . reverse
                     . takeWhile (/= start)
                     $ iterate (map M.!) end

kids :: String -> IO [String]
kids = fmap links . get . ("http://www.wikipedia.com/wiki/" ++)

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
