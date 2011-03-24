{-# LANGUAGE OverloadedStrings #-}
module Main where
import KonnFav
import Model
import Database.Persist
import Database.Persist.Sqlite
import Data.Time
import System.Locale
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as BE
import Data.Enumerator
import Network.HTTP.Enumerator
import Data.Attoparsec.Enumerator
import Data.Attoparsec.Lazy
import Data.Aeson hiding (Error)
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as L8
import Prelude hiding (concat)
import Control.Monad
import Control.Exception.Peel
import Web.Authenticate.OAuth hiding (insert)
import Control.Monad.IO.Class

twitter :: OAuth
twitter = OAuth { oauthServerName = "twitter"
                , oauthRequestUri = "http://twitter.com/oauth/request_token"
                , oauthAccessTokenUri = "http://twitter.com/oauth/access_token"
                , oauthAuthorizeUri = "http://twitter.com/oauth/authorize"
                , oauthSignatureMethod = HMACSHA1
                , oauthConsumerKey = pack consumerKey
                , oauthConsumerSecret = pack consumerSecret
                , oauthCallback = Nothing
                }

token :: Credential
token = Credential []


resultToMaybe :: Data.Aeson.Result a -> Maybe a
resultToMaybe (Success a) = Just a
resultToMaybe _           = Nothing

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = resultToMaybe . fromJSON

registerIter :: (MonadIO m, Functor m) => Iteratee Value (m) ()
registerIter = do
  mjs <- EL.head
  case mjs of
    Nothing -> return ()
    Just js -> do
      liftIO $ Prelude.putStrLn "hoge-"
      let fav = fromJSON' js :: Maybe Favouring
          tw  = fromJSON' js :: Maybe Tweet
          usr = fromJSON' js :: Maybe User
      liftIO $ withSqliteConn "debug.db3" $ runSqlConn $ do
        maybe (return ()) (void.insert) fav
        maybe (return ()) (void.insert) tw
        maybe (return ()) (void.insert) usr
      registerIter

enumLine :: Monad m => Enumeratee ByteString L8.ByteString m b
enumLine (Continue k) = do
  line <- BE.takeWhile (/= 10)
  BE.dropWhile (== 10)
  let chunk = if L8.null line then [] else [line]
  k (Chunks chunk) >>== enumLine
enumLine (Yield b st) = E.yield (Yield b st) (fmap (concat . L8.toChunks) st)
enumLine (Error e) = returnI $ Error e

(=$) :: Monad m => Enumeratee ao ai m b -> Iteratee ai m b -> Iteratee ao m b
(=$) = (joinI .) . ($$)
infixr 0 =$

main :: IO ()
main = do
  req <- signOAuth twitter token =<< parseUrl "https://userstream.twitter.com/2/user.json"
  run_ (http req $ \_ _ -> E.sequence (enumLine =$ E.map (concat . L8.toChunks) =$ iterParser json) =$ registerIter)
