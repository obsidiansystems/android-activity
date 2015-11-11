{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Focus.HTTP.Serve where

import Focus.HTTP.Accept

import Snap
import Snap.Util.FileServe

import Control.Exception (try, throwIO)
import Control.Monad
import Control.Monad.IO.Class
import Data.Attoparsec.ByteString (parseOnly, endOfInput)
import qualified Data.ByteString as BS
import Data.List
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import System.Directory
import System.FilePath
import System.IO.Error
import System.Posix (getFileStatus, fileSize)

import Debug.Trace.LocationTH

cachePermanently :: MonadSnap m => m ()
cachePermanently = do
  modifyResponse $ setHeader "Cache-Control" "public, max-age=315360000"
  modifyResponse $ setHeader "Expires" "Tue, 01 Feb 2050 00:00:00 GMT" --TODO: This should be set to "approximately one year from the time the response is sent"

doNotCache :: MonadSnap m => m ()
doNotCache = do
  modifyResponse $ setHeader "Cache-Control" "no-cache, no-store, must-revalidate" 
  modifyResponse $ setHeader "Expires" "0"

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
                Left err -> $failure err
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
          Right unknown -> $failure (T.unpack ("serveAssets': Unknown asset " <> decodeUtf8 unknown))
          Left err | isDoesNotExistError err -> pass
                   | otherwise -> liftIO $ $checkIO $ throwIO err
  go $ if "/" `isSuffixOf` pRaw || pRaw == "" then pRaw <> "index.html" else pRaw
