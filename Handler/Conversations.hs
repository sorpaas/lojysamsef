{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Conversations where

import Import
import LojysambanLib(ask, readRules, loadRules, end)

postAskR :: Text -> Handler Value
postAskR ident = do
  questionValueMaybe <- lookupPostParam "question"
  conversation <- listToMayBe $ getBy404 ConversationIdent ident
  maybe (object ["error" .= "something wrong happened."]) returnJSONAnswer questionValueMaybe
    where returnJSONAnswer Just q = ask q $ loadRules (conversationMessages conversation)
