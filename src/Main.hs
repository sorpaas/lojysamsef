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
import Snap.Extras.JSON(writeJSON)
import Data.ByteString.Char8(pack)
import Data.Text.Encoding(decodeUtf8)
import Data.Text(unpack)
import Data.Aeson

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

data Person  = Person {
  name :: String
  } deriving (Show)
instance ToJSON Person where
  toJSON (Person s) = object ["name" .= s]

data AnswerResult = AnswerResult {
  result :: String
  } deriving (Show)
instance ToJSON AnswerResult where
  toJSON (AnswerResult s) = object ["result" .= s]

data AnswerError = AnswerError {
  error :: String
  } deriving (Show)
instance ToJSON AnswerError where
  toJSON (AnswerError s) = object ["error" .= s]

data Info = Info {
  application :: String
  } deriving (Show)
instance ToJSON Info where
  toJSON (Info s) = object ["application" .= s]

site :: Snap ()
site =
    ifTop (writeJSON $ Person "me") <|>
    route [ ("info", writeJSON $ Info "lojysamsef")
          , ("ask", askHandler)
          ] <|>
    dir "static" (serveDirectory ".")

answerQuestion :: String -> Maybe String
answerQuestion q =
  ask q $ readRules ".i la .iocikun. patfu la .ituk. .i la manam. mamta la .ituk. .i da rirni de .ijanai da patfu de .i da rirni de .ijanai da mamta de"

askHandler :: Snap ()
askHandler = do
    param <- getParam "question"
    maybe (writeJSON $ AnswerError "must specify echo/param in URL")
          (\s -> (maybe
           (writeJSON $ AnswerError "no result")
           (\t -> (writeJSON $ AnswerResult t))
           (answerQuestion $ (unpack . decodeUtf8) s)
          )) param

-- (answerQuestion $ (unpack . decodeUtf8) (maybe (pack "ma rirni la .ituk.") (\s -> s) param)
-- (unpack . decodeUtf8) param -> String
