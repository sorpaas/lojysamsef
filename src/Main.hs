{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO(hFlush, stdout)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import Control.Applicative((<$>))
import Control.Arrow((&&&))
import Control.Monad.Tools(doWhile, doWhile_)
import LojysambanLib(ask, readRules, end)
import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Data.ByteString.Char8(pack)
import Data.Text.Encoding(decodeUtf8)
import Data.Text(unpack)

main :: IO ()
-- main = do
--         args <- getArgs
--         rules <- readRules <$> case args of
--                 [] -> doWhile "" $ \s -> (id &&& not . end) . (s ++) <$> getLine
--                 [fp] -> readFile fp
--                 _ -> putStrLn "Usage: lojysamban [FILEPATH]" >> exitFailure
--         doWhile_ $ do
--                 q <- putStr ".i " >> hFlush stdout >> getLine
--                 maybe (return False) ((>> return True) . putStrLn) $ ask q rules


-- main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

answerQuestion :: String -> Maybe String
answerQuestion q =
  ask q $ readRules ".i la .iocikun. patfu la .ituk. .i la manam. mamta la .ituk. .i da rirni de .ijanai da patfu de .i da rirni de .ijanai da mamta de"

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "question"
    maybe (writeBS "must specify echo/param in URL")
          (writeBS . pack) (answerQuestion $ (unpack . decodeUtf8) (maybe (pack "ma rirni la .ituk.") (\s -> s) param))
