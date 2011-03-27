{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, ScopedTypeVariables #-}
module Handler.UserPage where

import KonnFav
import Control.Applicative
import Data.Int
import Control.Monad
import Data.Maybe
import Data.String

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
      tweets <- mapM (favWithUsers . snd) tws
      return (user, filter (not . null . snd) tweets)
    tweets <- mapM renderTweet tws
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle $ fromString $ "Rescently faved tweets of " ++ scrName
        addWidget $(widgetFile "userpage")

getFavouringsR :: String -> Handler RepHtml
getFavouringsR  scrName = do
    (user, tws) <- runDB $ do
      (_, user) <- getBy404 (UserScreenName scrName)
      favs <- map snd <$> selectList [FavouringFromEq (userUserId user)] [] 0 0
      tws <- mapM (favWithUsers.snd) =<< mapM (getBy404 . UniqueTweet .favouringTweet) favs
      return (user, tws)
    tweets <- mapM renderTweet tws
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle $ fromString $ "Favourites of " ++ scrName
        addWidget $(widgetFile "userpage")
