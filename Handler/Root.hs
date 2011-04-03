{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import KonnFav
import Utils
import Control.Applicative
import Data.Enumerator hiding (mapM, consume)
import qualified Data.Enumerator as E
import Data.Enumerator.List hiding (mapM)
import qualified Data.Enumerator.List as EL
import Control.Monad

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
      run_ $ tweets $$ EL.mapM (favWithUsers.snd) =$ EL.filter (not . null . snd) =$
               EL.filterM (isProtectedTweet.fst) =$ isolate 20 =$ consume
    tweets <- mapM renderTweet tws
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "konnfav homepage"
        addWidget $(widgetFile "homepage")

isProtectedTweet :: (PersistBackend m, Functor m) => Tweet -> m Bool
isProtectedTweet tw = do
  musr <- liftM (userProtected . snd) <$> getBy (UserScreenName $ tweetUser tw)
  case musr of
    Nothing -> return False
    Just pr -> return pr
