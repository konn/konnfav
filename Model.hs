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
import System.Locale

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
    createdAt TwitterTime Lt Gt Ge Le Desc
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

newtype TwitterTime = TwitterTime { fromTwitterTime :: UTCTime }
    deriving (Eq, Ord, Show, Read, PersistField)

instance FromJSON User where
  parseJSON (Object v) = User <$> v .:  "screen_name" <*> v .: "id"
                              <*> v .:? "profile_image_url"
                     <|> (parseJSON =<< v .: "user")
  parseJSON _          = mzero


instance FromJSON Tweet where
  parseJSON (Object v) =
      Tweet <$> v .: "text" <*> v .: "created_at" <*> v .: "id"
            <*> (v .: "user" >>= (.: "screen_name"))
            <*> v .:? "in_reply_to_status_id" <*> v .:? "in_reply_to_user_id"
  parseJSON _          = mzero


instance FromJSON Favouring where
  parseJSON (Object v) = do
    ev <- v .: "event"
    if (ev == ("favorite" :: Text)) 
      then Favouring <$> (userUserId <$> v .: "source")
                     <*> (tweetStatusId <$> v .: "target_object")
      else mzero
  parseJSON _          = mzero


instance FromJSON TwitterTime where
  parseJSON (String t) =
    case parseTime defaultTimeLocale "%c" (unpack t) of
      Just t  -> return $ TwitterTime t
      Nothing -> fail "parsing twitter time format fail"
  parseJSON _          = mzero


instance ToJSON TwitterTime where
  toJSON = String . pack . formatTime defaultTimeLocale "%c" . fromTwitterTime

instance ToJSON User where
  toJSON usr = object [ "screen_name" .= userScreenName usr
                      , "id" .= userUserId usr
                      , "profile_image_url" .= userIcon usr
                      ]

instance ToJSON Tweet where
  toJSON tw =
    object [ "text" .= tweetText tw
           , "created_at" .= tweetCreatedAt tw
           , "id" .= tweetStatusId tw
           , "user" .= object []
           ]


