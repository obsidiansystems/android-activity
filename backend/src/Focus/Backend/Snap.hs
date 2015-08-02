{-# LANGUAGE OverloadedStrings #-}
module Focus.Backend.Snap where

import Snap
import Snap.Util.FileServe
import System.FilePath
import Data.String
import Data.Monoid

error404 :: MonadSnap m => m ()
error404 = do
  modifyResponse $ setResponseCode 404
  writeBS "404 Not Found"

ensureSecure :: Int -> Handler a b () -> Handler a b ()
ensureSecure port h = do
  s <- getsRequest rqIsSecure
  if s then h else do
    uri <- getsRequest rqURI
    host <- getsRequest rqServerName --TODO: It might be better to use the canonical base of the server
    redirect $ "https://" <> host <> (if port == 443 then "" else ":" <> fromString (show port)) <> uri

serveApp :: MonadSnap m => String -> m ()
serveApp app = do
  modifyResponse $ setHeader "Cache-Control" "no-cache, no-store, must-revalidate" 
  modifyResponse $ setHeader "Expires" "0"
  route [ ("", serveDirectory $ app </> "static")
        , ("", serveDirectory $ app </> "frontend.jsexe")
        , ("", error404)
        ]
