{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Focus.Backend.Snap where

import Focus.Backend.HTTP.Accept

import Snap
import Snap.Util.FileServe
import System.FilePath
import Data.String
import Data.Monoid
import Lucid
import Diagrams.Prelude (Diagram, renderDia, mkWidth)
import Diagrams.Backend.SVG
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Control.Lens
import Text.RawString.QQ
import Control.Exception (try)
import System.IO.Error
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad.IO.Class
import System.Directory
import System.Posix (getFileStatus, fileSize)
import Control.Monad
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Attoparsec.ByteString (parseOnly, endOfInput)

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

serveAppAt :: MonadSnap m => ByteString -> FilePath -> AppConfig -> m ()
serveAppAt loc app cfg = do
  route [ (loc, ifTop $ serveIndex cfg)
        , (loc, serveAssets (app </> "assets"))
        , (loc, doNotCache >> serveDirectory (app </> "static"))
        , (loc, serveAssets (app </> "frontend.jsexe.assets"))
        , (loc, doNotCache >> serveDirectory (app </> "frontend.jsexe"))
        , (loc, doNotCache >> error404)
        ]

serveApp :: MonadSnap m => FilePath -> AppConfig -> m ()
serveApp = serveAppAt ""

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
      script_ [type_ "text/javascript", src_ (T.pack "all.js"), defer_ "defer"] ("" :: String)

serveAssets :: MonadSnap m => FilePath -> m ()
serveAssets = serveAssets' True

serveAssetsInPlace :: MonadSnap m => FilePath -> m ()
serveAssetsInPlace = serveAssets' False

serveAssets' :: MonadSnap m => Bool -> FilePath -> m ()
serveAssets' doRedirect base = do
  pRaw <- getSafePath
  let go p = do
        assetType <- liftIO $ try $ BS.readFile $ base </> p </> "type"
        case assetType of
          Right "immutable" -> do
            encodedFiles <- liftM (filter (`notElem` [".", ".."])) $ liftIO $ getDirectoryContents $ base </> p </> "encodings"
            availableEncodings <- liftM (map snd . sort) $ forM encodedFiles $ \f -> do
              stat <- liftIO $ getFileStatus $ base </> p </> "encodings" </> f
              return (fileSize stat, Encoding $ encodeUtf8 $ T.pack f)
            acceptEncodingRaw <- getsRequest $ getHeader "Accept-Encoding"
            ae <- case acceptEncodingRaw of
              Nothing -> return missingAcceptableEncodings
              Just aer -> case parseOnly (acceptEncodingBody <* endOfInput) aer of
                Right ae -> return ae
            Just (Encoding e) <- return $ chooseEncoding availableEncodings ae
            modifyResponse $ setHeader "Content-Encoding" e . setHeader "Vary" "Accept-Encoding"
            if doRedirect then cachePermanently else doNotCache --TODO: Use Etags when not redirecting
            let finalFilename = base </> p </> "encodings" </> T.unpack (decodeUtf8 e)
            stat <- liftIO $ getFileStatus finalFilename
            modifyResponse $ setResponseCode 200 . setContentLength (fromIntegral $ fileSize stat) . setContentType (fileType defaultMimeTypes p)
            sendFile finalFilename
          Right "redirect" -> do
            target <- liftIO $ BS.readFile $ base </> p </> "target"
            if doRedirect
              then do
                doNotCache
                redirect target
              else go $ takeDirectory p </> T.unpack (decodeUtf8 target)
          Left err | isDoesNotExistError err -> pass
  go $ if "/" `isSuffixOf` pRaw || pRaw == "" then pRaw <> "index.html" else pRaw

makeLenses ''AppConfig
