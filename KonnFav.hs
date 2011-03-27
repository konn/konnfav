{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module KonnFav
    ( KonnFav (..)
    , KonnFavRoute (..)
    , resourcesKonnFav
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , module Yesod
    , module Settings
    , module Model
    , StaticRoute (..)
    , AuthRoute (..)
    , renderTweet, favWithUsers, getSearchR
    ) where

import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.OAuth
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Database.Persist.GenericSql
import Settings ( hamletFile, cassiusFile, juliusFile, widgetFile
                , consumerKey, consumerSecret)
import Model
import Data.Maybe (isJust)
import Control.Monad (join, unless)
import Network.Mail.Mime
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import Data.Maybe
import Text.Jasmine (minifym)
import Network.HTTP.Enumerator
import Web.Authenticate.OAuth (signOAuth, Credential(..))
import Data.Aeson
import Data.Attoparsec.Lazy
import Control.Applicative
import TwitterSettings
import Control.Arrow ((***))
import Data.ByteString.UTF8 (fromString)
import Control.Monad

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data KonnFav = KonnFav
    { getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Settings.ConnectionPool -- ^ Database connection pool.
    }

-- | A useful synonym; most of the handler functions in your application
-- will need to be of this type.
type Handler = GHandler KonnFav KonnFav

-- | A useful synonym; most of the widgets functions in your application
-- will need to be of this type.
type Widget = GWidget KonnFav KonnFav

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype KonnFavRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route KonnFav = KonnFavRoute
-- * Creates the value resourcesKonnFav which contains information on the
--   resources declared below. This is used in Controller.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- KonnFav. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the KonnFavRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "KonnFav" [parseRoutes|
/static StaticR Static getStatic
/auth   AuthR   Auth   getAuth

/users/#String FavedR GET
/users/#String/favs FavouringsR GET
/search SearchR GET

/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/ RootR GET
|]

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod KonnFav where
    approot _ = Settings.approot

    defaultLayout widget = do
        mu <- maybeAuth
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            widget
            addCassius $(Settings.cassiusFile "default-layout")
        hamletToRepHtml $(Settings.hamletFile "default-layout")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride a (StaticR s) =
        Just $ uncurry (joinPath a Settings.staticroot) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : ext'
        let content' =
                if ext' == "js"
                    then case minifym content of
                            Left _ -> content
                            Right y -> y
                    else content
        let statictmp = Settings.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content'
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

-- How to run database actions.
instance YesodPersist KonnFav where
    type YesodDB KonnFav = SqlPersist
    runDB db = liftIOHandler
             $ fmap connPool getYesod >>= Settings.runConnectionPool db

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = resultToMaybe . fromJSON

resultToMaybe :: Data.Aeson.Result a -> Maybe a
resultToMaybe (Success a) = Just a
resultToMaybe _           = Nothing

instance YesodAuth KonnFav where
    type AuthId KonnFav = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UserScreenName $ credsIdent creds
        let twCredential = Credential . map (fromString *** fromString) $ credsExtra creds
        case x of
            Just (uid, _) -> return $ Just uid
            Nothing -> do
                musr <- liftIO $ do
                  req <- parseUrl $ "http://api.twitter.com/1/users/show.json?screen_name=" ++ credsIdent creds
                  src <- responseBody <$> (httpLbs =<< signOAuth twitter twCredential req)
                  return $ fromJSON' =<< maybeResult (parse json src)
                case musr of
                  Nothing  -> return Nothing
                  Just usr -> Just <$> insert usr

    showAuthId _ = showIntegral
    readAuthId _ = readIntegral

    authPlugins = [ authTwitter consumerKey consumerSecret ]

renderTweet :: (Tweet, [User]) -> Handler (Widget ())
renderTweet tw = do
  muser <- runDB $ getBy (UserScreenName $ tweetUser $ fst tw)
  let user = maybe undefined snd muser
  return $(widgetFile "fav")

-- favWithUsers :: (PersistBackend m) => Tweet -> m (Tweet, [User])
favWithUsers tw = do
  favs <- selectList [FavouringTweetEq (tweetStatusId tw)] [] 0 0
  favers <- forM favs $ \(_, fav) -> snd <$> getBy404 (UserIdentifier $ favouringFrom fav)
  return (tw, filter (not . userProtected) favers)

getSearchR :: Handler ()
getSearchR = do
  scrName <- runFormGet' $ stringInput "screen_name"
  redirect RedirectTemporary $ FavedR scrName
