{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

-------------------------------------------------------------------------------
-- | 
-- Module      :  Focus.Backend.Snap
-- Copyright   :  
-- License     :  
-- Maintainer  :  obsidian.systems
-- Portability :  portable
-------------------------------------------------------------------------------

module Focus.Backend.Snap 
    ( AppConfig
    , serveAppAt
    , serveApp
    , serveStaticIndex
    , serveIndex
    , appConfig_initialHead
    , appConfig_initialBody
    )where

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
import qualified Graphics.Svg as SVG
import Graphics.Svg.Attributes
import Lucid
import System.FilePath
import Text.RawString.QQ
import Control.Monad.IO.Class


{- | Takes a port number and session handle to determine if what has been 
 - passed is an HTTPS Session. If not, it will request URI along with the name
 - of host and return a HTTPS redirect. -}
ensureSecure :: Int -> Handler a b () -> Handler a b ()
ensureSecure port h = do
  s <- getsRequest rqIsSecure
  if s then h else do
    uri <- getsRequest rqURI
    host <- getsRequest rqHostName --TODO: It might be better to use the canonical base of the server
    redirect $ "https://" <> host <> (if port == 443 then "" else ":" <> fromString (show port)) <> uri

-- | Data type for app configuration
data AppConfig m
   = AppConfig { _appConfig_logo :: Diagram SVG
               , _appConfig_extraHeadMarkup :: Html ()
               , _appConfig_initialStyles :: Maybe Text
               , _appConfig_initialBody :: Maybe (m ByteString)
               , _appConfig_initialHead :: Maybe ByteString
               , _appConfig_serveJsexe :: Bool
               , _appConfig_jsexe :: FilePath
               }

-- | Default instance for app configuration
instance Default (AppConfig m) where
  def = AppConfig { _appConfig_logo = mempty
                  , _appConfig_extraHeadMarkup = mempty
                  , _appConfig_initialStyles = mempty
                  , _appConfig_initialBody = Nothing
                  , _appConfig_initialHead = mempty
                  , _appConfig_serveJsexe = True
                  , _appConfig_jsexe = "frontend.jsexe"
                  }

-- | Takes a location, app, and an app configuration as arguements and
-- routes the request to the correct handler. If "appConfig_serveJsexe"
-- returns "True", additional js paths and asset paths will be appended to
-- handler. Otherwise, it will return "404 Not Found"
serveAppAt :: MonadSnap m => ByteString -> FilePath -> AppConfig m -> m ()
serveAppAt loc app cfg = do
  route $ [ (loc, ifTop $ serveStaticIndex cfg)
          , (loc, serveAssets (app </> "static.assets") (app </> "static"))
          , (loc <> "/version", doNotCache >> serveFileIfExistsAs "text/plain" (app </> "version"))
          ]
       ++ if _appConfig_serveJsexe cfg
            then [(loc, serveAssets (app </> frontendJsAssetsPath cfg) (app </> frontendJsPath cfg))]
            else []
       ++ [ (loc, doNotCache >> error404) ]

-- | Writes a 404 error message in ByteString
error404 :: MonadSnap m => m ()
error404 = do
  modifyResponse $ setResponseCode 404
  writeBS "404 Not Found"

-- | Partial serveAppAt function whose awaiting to be passed an AppConfig
-- argument
serveApp :: MonadSnap m => FilePath -> AppConfig m -> m ()
serveApp = serveAppAt ""

-- | Takes an AppConfig argument and returns a frontendJs/... filepath 
frontendJsPath :: AppConfig m -> FilePath
frontendJsPath (AppConfig { _appConfig_jsexe = jsexe }) = "frontendJs" </> jsexe

-- | Takes an AppConfig argument and returns a frontendJs.assets/... filepath
frontendJsAssetsPath :: AppConfig m -> FilePath
frontendJsAssetsPath (AppConfig { _appConfig_jsexe = jsexe }) = "frontendJs.assets" </> jsexe

-- | Takes an AppConfig monad and uses it to generate & write Lazy ByteString
-- to the body of the https response
serveStaticIndex :: MonadSnap m => AppConfig m -> m ()
serveStaticIndex cfg = do
  appJsPath <- liftIO $ getAssetPath (frontendJsAssetsPath cfg) "/all.js"
  initialBody <- fromMaybe (return "") $ _appConfig_initialBody cfg
  let initialHead = fromMaybe "" $ _appConfig_initialHead cfg
  let initialStyles = fromMaybe "" $ _appConfig_initialStyles cfg

  writeLBS $ renderBS $ doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      _appConfig_extraHeadMarkup cfg
      toHtmlRaw initialHead
      style_ initialStyles
    body_ $ do
      toHtmlRaw initialBody
      script_ [type_ "text/javascript", src_ (maybe "/all.js" T.pack appJsPath), defer_ "defer"] ("" :: String)
      return ()

-- | Takes an AppConfig monad and uses it to generate & write Lazy ByteString
-- to the body of the https response. This one comes with hard coded values
-- for the logo 
serveIndex :: MonadSnap m => AppConfig m -> m ()
serveIndex cfg = do
  appJsPath <- liftIO $ getAssetPath (frontendJsAssetsPath cfg) "all.js"
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
      let svgOpts = SVGOptions (mkWidth 400) Nothing "preload-logo-" [bindAttr Id_ "preload-logo"] False
      toHtml $ SVG.renderBS $ renderDia SVG svgOpts $ _appConfig_logo cfg
      script_ [type_ "text/javascript", src_ (maybe "/all.js" T.pack appJsPath), defer_ "defer"] ("" :: String)

makeLenses ''AppConfig
