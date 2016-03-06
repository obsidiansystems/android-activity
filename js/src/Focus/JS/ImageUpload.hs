module Focus.JS.ImageUpload where

import Reflex.Dom
import Focus.JS.FileReader
import Data.List
import Data.Maybe
import Data.Monoid

-- Basically just an example of how to use imageUploadWidget
simpleImageUploadWidget :: (MonadWidget t m)
                        => String -- ^ URL to which we should POST image file
                        -> m (Event t XhrResponse)
simpleImageUploadWidget postUrl =
  imageUploadWidget (XhrRequest "POST" postUrl def) $ \url -> do
    e <- button "Upload"
    el "div" . elAttr "img" ("src" =: url <> "style" =: "max-width: 80%") $ blank
    return e

imageUploadWidget :: (MonadWidget t m)
                  => XhrRequest () -- ^ request template with blank data
                  -> (String -> m (Event t ())) -- ^ image preview widget (function of data URL) which gives upload trigger event
                  -> m (Event t XhrResponse)
imageUploadWidget reqTemplate preview = do
  filesDyn <- value <$> fileInput def
  let fileE = fmapMaybe listToMaybe . updated $ filesDyn
  currentFile <- hold Nothing (fmap Just fileE)
  -- Is the following really the best way to tell it's an image?
  urlE <- fmap (ffilter ("data:image" `isPrefixOf`)) . dataURLFileReader $ fileE
  fmap (switch . current) . widgetHold (return never) . ffor urlE $ \url -> do
    uploadE <- preview url
    blobE <- blobFileReader (fmapMaybe id (tag currentFile uploadE))
    let request = ffor blobE $ \b ->
                    reqTemplate & xhrRequest_config . xhrRequestConfig_sendData .~ b
    performRequestAsync request

