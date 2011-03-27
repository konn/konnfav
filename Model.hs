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
import Data.Maybe

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
share2 mkPersist (mkMigrate "migrateAll") [persist|
User
    screenName String Eq
    userId Word64 Eq
    protected Bool Eq
    description String default=''
    favouritesCount Int
    followersCount Int
    friendsCount Int
    geoEnabled Bool
    lang String default='en'
    listedCount Int
    location String Maybe
    name String Maybe
    statusesCount Int
    timezone String Maybe
    url String Maybe
    utcOffset Int Maybe
    verified Bool
    icon String Maybe
    UserScreenName screenName
    UserIdentifier userId
Tweet
    text Text
    createdAt TwitterTime Lt Gt Ge Le Desc
    statusId Word64 Eq
    user String Eq
    place String Maybe
    source String default='Web'
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
  parseJSON (Object v) = User <$> v .:  "screen_name"
                              <*> v .: "id"
                              <*> v .:  "protected"
                              <*> (fromMaybe "" <$> v .:?  "description")
                              <*> v .:  "favourites_count"
                              <*> v .:  "followers_count"
                              <*> v .:  "friends_count"
                              <*> v .:  "geo_enabled"
                              <*> v .:  "lang"
                              <*> v .:  "listed_count"
                              <*> v .:? "location"
                              <*> v .:? "name"
                              <*> v .:  "statuses_count"
                              <*> v .:? "time_zone"
                              <*> v .:? "url"
                              <*> v .:? "utc_offset"
                              <*> v .:  "verified"
                              <*> v .:? "profile_image_url"
                     <|> (parseJSON =<< v .: "user")
  parseJSON _          = mzero


instance FromJSON Tweet where
  parseJSON (Object v) =
      Tweet <$> v .: "text" <*> v .: "created_at" <*> v .: "id"
            <*> (v .: "user" >>= (.: "screen_name"))
            <*> v .:? "place" <*> v .: "source"
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
                      , "protected" .= userProtected usr
                      , "description" .= userDescription usr
                      , "favourites_count" .= userFavouritesCount usr
                      , "followers_count" .= userFollowersCount usr
                      , "friends_count" .= userFriendsCount usr
                      , "geo_enabled" .= userGeoEnabled usr
                      , "lang" .= userLang usr
                      , "listed_count" .= userListedCount usr
                      , "location" .= userLocation usr
                      , "name" .= userName usr
                      , "statuses_count" .= userStatusesCount usr
                      , "time_zone" .= userTimezone usr
                      , "url" .= userUrl usr
                      , "utc_offset" .= userUtcOffset usr
                      , "verified" .= userVerified usr
                      , "profile_image_url" .= userIcon usr
                      ]

instance ToJSON Tweet where
  toJSON tw =
    object [ "text" .= tweetText tw
           , "created_at" .= tweetCreatedAt tw
           , "id" .= tweetStatusId tw
           , "user" .= object []
           , "place" .= tweetPlace tw
           , "source" .= tweetSource tw
           , "in_reply_to_status_id" .= tweetInReplyToStatusId tw
           , "in_reply_to_user_id" .= tweetInReplyToUserId tw
           ]


