{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Status where
import KonnFav
import Data.String
import Data.Text (unpack)
import Data.Maybe

getStatusR :: Integer -> Handler RepHtml
getStatusR sid = do
  (user, stat) <- runDB $ do
    (_, stat) <- getBy404 $ UniqueTweet (fromInteger sid)
    st <- favWithUsers stat
    (_, user) <- getBy404 $ UserScreenName (tweetUser stat)
    return (user, st)
  status <- renderTweet stat
  defaultLayout $ do
    setTitle $ fromString $ "@" ++ userScreenName user ++ ":" ++ unpack (tweetText $ fst stat)
    addWidget $(widgetFile "status")
