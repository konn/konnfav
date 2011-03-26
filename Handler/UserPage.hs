{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, ScopedTypeVariables #-}
module Handler.UserPage where

import KonnFav
import Control.Applicative
import Data.Int
import Control.Monad
import Data.Maybe

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- KonnFav.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getFavedR :: String -> Handler RepHtml
getFavedR scrName = do
    (user, tws) <- runDB $ do
      (_, user) <- getBy404 (UserScreenName scrName)
      tws <- selectList [TweetUserEq scrName] [TweetCreatedAtDesc] 0 0
      tweets <- forM tws $ \(tid, tw) -> do
          favs <- selectList [FavouringTweetEq (tweetStatusId tw)] [] 0 0
          (,) tw <$> forM favs (\(_, fav) -> snd <$> getBy404 (UserIdentifier $ favouringFrom fav))
      return (user, filter (not . null . snd) tweets)
    let tweets =  tws
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "konnfav homepage"
        addWidget $(widgetFile "userpage")

getFavouringsR :: String -> Handler RepHtml
getFavouringsR  scrName = do
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "konnfav homepage"
        addWidget $(widgetFile "homepage")
