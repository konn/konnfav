{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
module Model where

import Yesod
import Database.Persist.TH (share2)
import Database.Persist.GenericSql (mkMigrate)
import Data.Text
import Data.Time
import Data.Word
import Data.Aeson
import Control.Applicative
import Control.Monad

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
share2 mkPersist (mkMigrate "migrateAll") [persist|
User
    screenName String Eq
    userId Word64 Eq
    icon String Maybe
    UserScreenName screenName
    UserIdentifier userId
Tweet
    text Text
    createdAt UTCTime Lt Gt Ge Le Desc
    statusId Word64 Eq
    user String Eq
    inReplyToStatusId Word64 Maybe
    inReplyToUserId   Word64 Maybe
    UniqueTweet statusId    
Favouring
    from Word64 Eq
    tweet Word64 Eq
    UniqueFavouring from tweet
|]

instance FromJSON User where
  parseJSON (Object v) = User <$> v .:  "screen_name" <*> v .: "id"
                              <*> v .:? "profile_image_url"
                     <|> (parseJSON =<< v .: "user")
  parseJSON _          = mzero

instance FromJSON Tweet where
  parseJSON (Object v) =
      Tweet <$> v .: "text" <*> v .: "created_at" <*> v .: "status_id"
            <*> (v .: "user" >>= (.: "screen_name"))
            <*> v .:? "in_reply_to_status_id" <*> v .:? "in_reply_to_user_id"
  parseJSON _          = mzero


instance FromJSON Favouring where
  parseJSON (Object v) = do
    ev <- v .: "event"
    if (ev == ("favorite" :: Text)) 
      then Favouring <$> v .: "source" <*> v .: "target"
      else mzero
  parseJSON _          = mzero

