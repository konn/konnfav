{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import KonnFav
import Utils
import Control.Applicative
import Data.Enumerator hiding (mapM, consume)
import qualified Data.Enumerator as E
import Data.Enumerator.List

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
      let tweets = select [] [TweetCreatedAtDesc] 0 0
      run_ (tweets $$ E.mapM (favWithUsers.snd) =$ E.filter (not . null . snd) =$ isolate 20 =$ consume)
    tweets <- mapM renderTweet tws
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "konnfav homepage"
        addWidget $(widgetFile "homepage")
