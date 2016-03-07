module Focus.JS.ImageUpload where

import Reflex.Dom
import Focus.JS.FileReader
import Data.List
import Data.Maybe
import Data.Monoid

simpleImageUploadWidget :: (MonadWidget t m)
                        => String -- ^ URL to which we should POST image file
                        -> m (Event t XhrResponse)
simpleImageUploadWidget postUrl =
  imageUploadWidget (XhrRequest "POST" postUrl def) (button "Upload") $ \url ->
    el "div" . elAttr "img" ("src" =: url <> "style" =: "max-width: 80%") $ blank

imageUploadWidget :: (MonadWidget t m)
                  => XhrRequest () -- ^ request template with blank data
                  -> m (Event t ()) -- ^ upload button widget
                  -> (String -> m ()) -- ^ image preview widget (function of data URL)
                  -> m (Event t XhrResponse)
imageUploadWidget reqTemplate uploadBtn preview = do
  filesDyn <- value <$> fileInput def
  let fileE = fmapMaybe listToMaybe . updated $ filesDyn
  currentFile <- hold Nothing (fmap Just fileE)
  -- Is the following really the best way to tell it's an image?
  urlE <- fmap (ffilter ("data:image" `isPrefixOf`)) . dataURLFileReader $ fileE
  fmap (switch . current) . widgetHold (return never) . ffor urlE $ \url -> do
    preview url
    uploadE <- uploadBtn
    blobE <- blobFileReader (fmapMaybe id (tag currentFile uploadE))
    let request = ffor blobE $ \b ->
                    reqTemplate & xhrRequest_config . xhrRequestConfig_sendData .~ b
    performRequestAsync request

