{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import KonnFav
import Control.Applicative

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- KonnFav.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    tws <- runDB $ do
      dics <- selectList [] [TweetCreatedAtDesc] 30 1
      take 20 . filter (not . null . snd) <$> mapM (favWithUsers.snd) dics
    tweets <- mapM renderTweet tws
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "konnfav homepage"
        addWidget $(widgetFile "homepage")

