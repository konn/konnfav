{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NamedFieldPuns #-}
module Main where
import KonnFav
import Model
import Database.Persist
import Database.Persist.Sqlite
import Database.Sqlite
import Data.Time
import System.Locale
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as BE
import Data.Enumerator (Enumeratee(..), Iteratee(..), joinI, ($$), Step(..), Stream(..)
                       , returnI, (>>==), run_)
import Network.HTTP.Enumerator
import Data.Attoparsec.Enumerator
import Data.Attoparsec.Lazy
import Control.Applicative
import Data.Aeson hiding (Error)
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as L8
import Prelude hiding (concat, catch)
import Control.Monad
import Control.Exception.Peel
import Web.Authenticate.OAuth hiding (insert, delete)
import Control.Monad.IO.Class
import Control.Concurrent
import System.Random
import Data.Word
import Settings

resultToMaybe :: Data.Aeson.Result a -> Maybe a
resultToMaybe (Success a) = Just a
resultToMaybe _           = Nothing

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = resultToMaybe . fromJSON

registerIter :: Iteratee Value (IO) ()
registerIter = do
  mjs <- EL.head
  case mjs of
    Nothing -> return ()
    Just js -> do
      liftIO $ Prelude.putStrLn "hoge-"
      let fav = fromJSON' js :: Maybe Favouring
          tw  = fromJSON' js :: Maybe Tweet
          usr = fromJSON' js :: Maybe User
          ev  = fromJSON' js :: Maybe Event
      liftIO $ L8.putStrLn (encode js) >> print fav >> print tw >> print usr >> print ev
      liftIO $ handleBusy $ withSqliteConn "debug.db3" $ runSqlConn $ do
        maybe (return ()) (void.insertBy) fav
        maybe (return ()) (void.insertBy) tw
        maybe (return ()) (void.insertBy) usr
        maybe (return ()) procEvent ev
      registerIter

procEvent ev = do
  either (const $ return ()) (void.insertBy) (_source ev)
  either (const $ return ()) (void.insertBy) (_target ev)
  either (const $ return ()) (void.insertBy) (_targetObject ev)
  case event ev of
    "unfavorite" -> do
      liftIO $ print ev
      deleteBy $ UniqueFavouring (eventSource ev) (eventTargetObject ev)
    "delete"     -> do
      deleteBy $ UniqueTweet (eventTargetObject ev)
      mapM_ (delete . fst) =<< selectList [FavouringTweetEq (eventTargetObject ev)] [] 0 0
    _            -> return ()

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
  req <- signOAuth twitter tokenCredential =<< parseUrl "https://userstream.twitter.com/2/user.json"
  run_ (http req $ \_ _ -> E.sequence (enumLine =$ EL.map (concat . L8.toChunks) =$ iterParser json) =$ registerIter)

handleBusy :: (Functor m, MonadPeelIO m) => m a -> m ()
handleBusy act = void act `catch` handler
  where
    handler (e::SomeException) =
      case Prelude.take 1 $ Prelude.drop 4 $ Prelude.words $ show e of
        ["ErrorBusy"] -> liftIO (Prelude.putStrLn "Busy. retry..." >> randomRIO (1,1000) >>= threadDelay) >> handleBusy act
        _             -> liftIO (print e)

data Event = Event { event     :: String
                   , createdAt :: Maybe TwitterTime
                   , _source :: Either Word64 User
                   , _target :: Either Word64 User
                   , _targetObject :: Either Word64 Tweet
                   } deriving (Show, Eq)

eventSource, eventTarget, eventTargetObject :: Event -> Word64
eventSource = either id userUserId . _source
eventTarget = either id userUserId . _target
eventTargetObject = either id tweetStatusId . _targetObject

instance FromJSON Event where
  parseJSON (Object v) = Event <$> v .: "event"  <*> v .: "created_at"
                               <*> v .: "source"
                               <*> v .: "target" <*> v .: "target_object"
                      <|> (v .: "delete" >>= (.: "status") >>= \d -> (Event "delete" Nothing
                                                                      <$> d .: "user_id"
                                                                      <*> d .: "user_id" <*> d .: "id"))
  parseJSON _          = mzero

instance ToJSON Event where
  toJSON Event{event, _source, _target, _targetObject}
      = object [ "event"  .= event, "source" .= _source
               , "target" .= _target, "target_object" .= _targetObject
               ]
