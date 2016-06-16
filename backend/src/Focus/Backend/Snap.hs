{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Focus.Backend.Snap where

import Focus.HTTP.Serve

import Snap

import Control.Lens
import Data.ByteString (ByteString)
import Data.Default
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Diagrams.Prelude (Diagram, renderDia, mkWidth)
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.Attributes
import Lucid
import System.FilePath
import Text.RawString.QQ
import Control.Monad.IO.Class
import Lucid.Base (makeAttribute)

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

data AppConfig
   = AppConfig { _appConfig_logo :: Diagram SVG
               , _appConfig_extraHeadMarkup :: Html ()
               , _appConfig_initialStyles :: Maybe Text
               , _appConfig_initialBody :: Maybe ByteString
               , _appConfig_initialHead :: Maybe ByteString
               }

instance Default AppConfig where
  def = AppConfig { _appConfig_logo = mempty
                  , _appConfig_extraHeadMarkup = mempty
                  , _appConfig_initialStyles = mempty
                  , _appConfig_initialBody = mempty
                  , _appConfig_initialHead = mempty
                  }

frontendJsexeAssets :: FilePath
frontendJsexeAssets = "frontend.jsexe.assets"

serveAppAt :: MonadSnap m => ByteString -> FilePath -> AppConfig -> m ()
serveAppAt loc app cfg = do
  route [ (loc, ifTop $ serveIndex cfg)
        , (loc <> "/sr", ifTop $ serveStaticIndex cfg)
        , (loc, serveAssets (app </> "assets") (app </> "static"))
        , (loc, serveAssets (app </> frontendJsexeAssets) (app </> "frontend.jsexe"))
        , (loc <> "/version", doNotCache >> serveFileIfExistsAs "text/plain" (app </> "version"))
        , (loc, doNotCache >> error404)
        ]

serveApp :: MonadSnap m => FilePath -> AppConfig -> m ()
serveApp = serveAppAt ""

serveStaticIndex :: MonadSnap m => AppConfig -> m ()
serveStaticIndex cfg = do
  appJsPath <- liftIO $ getAssetPath frontendJsexeAssets "all.js"
  let initialBody = fromMaybe "" $ _appConfig_initialBody cfg
  let initialHead = fromMaybe "" $ _appConfig_initialHead cfg
  let initialStyles = fromMaybe "" $ _appConfig_initialStyles cfg

  writeLBS $ renderBS $ doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      _appConfig_extraHeadMarkup cfg
      style_ [r|
        html, body { height: 100%; width: 100%; }
      |]
      toHtmlRaw initialHead
      style_ initialStyles
    body_ $ do
      toHtmlRaw initialBody
      script_ [type_ "text/javascript", src_ (maybe "all.js" T.pack appJsPath), defer_ "defer"] ("" :: String)
      return ()


serveIndex :: MonadSnap m => AppConfig -> m ()
serveIndex cfg = do
  appJsPath <- liftIO $ getAssetPath frontendJsexeAssets "all.js"
  writeLBS $ renderBS $ doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      _appConfig_extraHeadMarkup cfg
      style_ [r|
        #preload-logo {
            position: fixed;
            left: 25%;
            top: 25%;
            width: 50%;
            height: 50%;
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
      let svgOpts = SVGOptions (mkWidth 400) Nothing "preload-logo-" [makeAttribute "id" "preload-logo"] False
      renderDia SVG svgOpts $ svgId "preload-logo" $ _appConfig_logo cfg
      script_ [type_ "text/javascript", src_ (maybe "all.js" T.pack appJsPath), defer_ "defer"] ("" :: String)

makeLenses ''AppConfig
