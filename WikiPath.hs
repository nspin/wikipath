{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Lens hiding (elements)
import           Control.Monad
import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Lens
import           Data.List
-- import qualified Data.Text as T
import           Data.Text.Lens
import           Data.Maybe
import qualified Data.Map as M
import           Network.Wreq
import           System.Environment
import           Text.Taggy.Lens

sleep = forever $ threadDelay maxBound

main = do
    [start, end] <- getArgs
    hist <- atomically $ newTVar M.empty
    let go prev curr = join . atomically $ do
            h <- readTVar hist
            if M.member curr h then return sleep else do
                let new = M.insert curr prev h
                writeTVar hist new
                return $ if curr == end
                         then print
                            . (start :)
                            . reverse
                            . takeWhile (/= start)
                            $ iterate (new M.!) end
                         else get ("http://www.wikipedia.com/wiki/" ++ curr) >>=
                               ( runConcurrently
                               . foldr (<|>) empty
                               . map (Concurrently . go curr)
                               . links
                               )
    go undefined start


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
