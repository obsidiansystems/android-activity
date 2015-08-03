{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Focus.Backend.Snap where

import Snap
import Snap.Util.FileServe
import System.FilePath
import Data.String
import Data.Monoid
import Lucid
import Diagrams.Prelude (Diagram, renderDia, mkWidth)
import Diagrams.Backend.SVG
import Data.Default
import Control.Lens
import Text.RawString.QQ

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

doNotCache :: MonadSnap m => m ()
doNotCache = do
  modifyResponse $ setHeader "Cache-Control" "no-cache, no-store, must-revalidate" 
  modifyResponse $ setHeader "Expires" "0"

cachePermanently :: MonadSnap m => m ()
cachePermanently = do
  modifyResponse $ setHeader "Cache-Control" "public, max-age=315360000"
  modifyResponse $ setHeader "Expires" "Tue, 01 Feb 2050 00:00:00 GMT" --TODO: This should be set to "approximately one year from the time the response is sent"

data AppConfig
   = AppConfig { _appConfig_logo :: Diagram SVG
               , _appConfig_extraHeadMarkup :: Html ()
               }

instance Default AppConfig where
  def = AppConfig { _appConfig_logo = mempty
                  , _appConfig_extraHeadMarkup = mempty
                  }

serveApp :: MonadSnap m => FilePath -> AppConfig -> m ()
serveApp app cfg = do
  route [ ("", ifTop $ serveIndex cfg)
--        , ("x", serveAssets (app </> "assets"))
        , ("", doNotCache >> serveDirectory (app </> "static"))
        , ("", doNotCache >> serveDirectory (app </> "frontend.jsexe"))
        , ("", doNotCache >> error404)
        ]

serveIndex :: MonadSnap m => AppConfig -> m ()
serveIndex cfg = do
  writeLBS $ renderBS $ doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      _appConfig_extraHeadMarkup cfg
      style_ [r|
        #preload-logo {
            -webkit-animation: fadein 2s; /* Safari, Chrome and Opera > 12.1 */
               -moz-animation: fadein 2s; /* Firefox < 16 */
                -ms-animation: fadein 2s; /* Internet Explorer */
                 -o-animation: fadein 2s; /* Opera < 12.1 */
                    animation: fadein 2s;
        }
        @keyframes fadein {
            from { opacity: 0; }
            to   { opacity: 1; }
        }
        /* Firefox < 16 */
        @-moz-keyframes fadein {
            from { opacity: 0; }
            to   { opacity: 1; }
        }
        /* Safari, Chrome and Opera > 12.1 */
        @-webkit-keyframes fadein {
            from { opacity: 0; }
            to   { opacity: 1; }
        }
        /* Internet Explorer */
        @-ms-keyframes fadein {
            from { opacity: 0; }
            to   { opacity: 1; }
        }
      |]
    body_ $ do
      let svgOpts = defaultSVGOptions (mkWidth 400)
                    & idAttr .~ Just "preload-logo"
                    & styleAttr .~ Just "position:fixed;left:25%;top:25%;width:50%;height:50%"
                    & idPrefix .~ "preload-logo-"
                    & generateDoctype .~ False
      renderDia SVG svgOpts $ _appConfig_logo cfg
      script_ [type_ "text/javascript", src_ "all.js", defer_ "defer"] ("" :: String)

serveAssets :: MonadSnap m => FilePath -> m ()
serveAssets _ = return ()

makeLenses ''AppConfig
