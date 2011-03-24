{-# LANGUAGE OverloadedStrings #-}
module Crawler where
import KonnFav
import Model
import Database.Persist
import Database.Persist.Sqlite
import Data.Time
import System.Locale
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Network.HTTP.Enumerator

time :: UTCTime
time = readTime defaultTimeLocale "%F %T" "2011-03-23 20:45:21"

main :: IO ()
main = withSqliteConn "debug.db3" $ runSqlConn $ do
  -- runMigration migrateAll
  mr_konn <- insert $ User "mr_konn" 5965172 (Just "http://a0.twimg.com/profile_images/748275234/myface_mosaic_normal.jpg")
  tw <- insert $ Tweet "論文共著数ナンバーワン！ et al. をどうぞよろしく" time 50523523432718336 mr_konn Nothing Nothing
  insert $ Favouring mr_konn tw
  liftIO $ print tw
