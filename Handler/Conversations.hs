{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Conversations where

import Import
import Data.Maybe
import Data.Text(unpack)
import LojysambanLib(ask, readRules, loadRules, end)

postAskR :: Text -> Handler Value
postAskR ident = do
  questionValueMaybe <- lookupPostParam "question"
--  let questionValue = fromJust questionValueMaybe in
--    return $ object ["question" .= questionValue]

--  conversation <- listToMaybe $ getBy404 ConversationIdent ident
  let questionValue = fromJust questionValueMaybe in
    return $ object ["answer" .= (returnJSONAnswer $ unpack questionValue)]
      where returnJSONAnswer q = maybe "nago'i" (\s -> s) $ ask q $ loadRules [".i da du da .i la .ualeis. cu nelci lo cirla .i la gromit. cu nelci lo cirla .i la .uendolen. cu nelci lo lanme .i da pendo de .ijanai tu'e da nadu de .i da nelci di .i de nelci di"]
