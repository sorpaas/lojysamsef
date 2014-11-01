{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad.Tools

import System.IO(hFlush, stdout)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import LojysambanLib(ask, readRules, end)
import Data.Text(unpack)
import Data.Text.Encoding(decodeUtf8)
import Data.ByteString.Char8(pack)

import qualified Snap.Core as Snap
import qualified Snap.Http.Server.Config as Snap
import qualified Snap.Snaplet as Snap
import qualified Snap.Util.FileServe as Snap
import qualified Snap.CORS as CORS
import qualified Snap.Extras.JSON as JSON
import qualified Data.Aeson as JSON

main :: IO ()
main = Snap.serveSnaplet Snap.defaultConfig site

site = Snap.makeSnaplet "app" "app" Nothing $ do
  Snap.addRoutes [ ("/", JSON.writeJSON $ AnswerResult "me")
                 , ("/info", JSON.writeJSON $ Info "lojysamsef")
                 , ("/ask", askHandler)]
  CORS.wrapCORS

askHandler = Snap.method Snap.POST $ do
    param <- Snap.getParam "question"
    maybe (JSON.writeJSON $ AnswerError "must specify echo/param in URL")
          (\s -> (maybe
           (JSON.writeJSON $ AnswerError "no result")
           (\t -> (JSON.writeJSON $ AnswerResult (show t)))
           (answerQuestion $ (unpack . decodeUtf8) s)
          )) param

answerQuestion :: String -> Maybe String
answerQuestion q =
  ask q $ readRules ".i da du da .i la .ualeis. cu nelci lo cirla .i la gromit. cu nelci lo cirla .i la .uendolen. cu nelci lo lanme .i da pendo de .ijanai tu'e da nadu de .i da nelci di .i de nelci di"

data Person = Person {
  name :: String
  } deriving (Show)
instance JSON.ToJSON Person where
  toJSON (Person s) = JSON.object ["name" JSON..= s]

data AnswerResult = AnswerResult {
  result :: String
  } deriving (Show)
instance JSON.ToJSON AnswerResult where
  toJSON (AnswerResult s) = JSON.object ["result" JSON..= s]

data AnswerError = AnswerError {
  error :: String
  } deriving (Show)
instance JSON.ToJSON AnswerError where
  toJSON (AnswerError s) = JSON.object ["error" JSON..= s]

data Info = Info {
  application :: String
  } deriving (Show)
instance JSON.ToJSON Info where
  toJSON (Info s) = JSON.object ["application" JSON..= s]

-- (answerQuestion $ (unpack . decodeUtf8) (maybe (pack "ma rirni la .ituk.") (\s -> s) param)
-- (unpack . decodeUtf8) param -> String

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
