{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
    ( withKonnFav
    ) where

import KonnFav
import Settings
import Yesod.Helpers.Static
import Yesod.Helpers.Auth
import Database.Persist.GenericSql
import Data.ByteString (ByteString)

-- Import all relevant handler modules here.
import Handler.Root
import Handler.UserPage

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in KonnFav.hs. Please see
-- the comments there for more details.
mkYesodDispatch "KonnFav" resourcesKonnFav

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withKonnFav :: (Application -> IO a) -> IO a
withKonnFav f = Settings.withConnectionPool $ \p -> do
    runConnectionPool (runMigration migrateAll) p
    let h = KonnFav s p
    toWaiApp h >>= f
  where
    s = static Settings.staticdir
