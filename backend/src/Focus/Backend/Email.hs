{-# LANGUAGE TemplateHaskell, FlexibleInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, UndecidableInstances, OverloadedStrings #-}
module Focus.Backend.Email where

import Focus.Brand
import Focus.Sign
import Focus.Route
import Focus.Backend.TH
import Network.Mail.Mime (Mail (..), htmlPart)
import Data.Text (Text)
import Text.Blaze.Html5 (Html)
import Data.List.NonEmpty (NonEmpty)
import Data.Foldable
import Text.Blaze.Html.Renderer.Text
import Network.Mail.SMTP (sendMailWithLogin', simpleMail, Address (..), UserName, Password)
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Reader
import Data.Monoid
import Network.Socket (PortNumber, HostName)
import Data.Aeson
import Data.Word
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Focus.TH (embedFile)
import Data.Text.Encoding
import Data.String (fromString)
import Data.Default

class Monad m => MonadEmail m where
  sendMail :: Mail -> m ()

type EmailEnv = (HostName, PortNumber, UserName, Password)

instance FromJSON PortNumber where
  parseJSON v = do
    n :: Word16 <- parseJSON v
    return $ fromIntegral n

instance ToJSON PortNumber where
  toJSON n = toJSON (fromIntegral n :: Word16)

newtype EmailT m a = EmailT { unEmailT :: ReaderT EmailEnv m a } deriving (Functor, Applicative, Monad, MonadIO, MonadRoute r, MonadSign, MonadBrand)

instance MonadIO m => MonadEmail (EmailT m) where
  sendMail mail = do
    (server, port, username, password) <- EmailT ask
    liftIO $ putStrLn $ "Sending email " <> show (map snd $ filter ((=="Subject") . fst) $ mailHeaders mail) <> " to " <> show (map addressEmail $ mailTo mail)
    liftIO $ sendMailWithLogin' server port username password mail

instance MonadEmail m => MonadEmail (ReaderT r m) where
  sendMail = lift . sendMail

runEmailT = runReaderT . unEmailT

sendEmailFrom :: MonadEmail m => Text -> Text -> NonEmpty Text -> Text -> Html -> m ()
sendEmailFrom name email recipients subject body = sendMail $ simpleMail (Address (Just name) email) (map (Address Nothing) $ toList recipients) [] [] subject [(htmlPart $ renderHtml body)]

deriveNewtypePersistBackend (\m -> [t| EmailT $m |]) (\m -> [t| ReaderT EmailEnv $m |]) 'EmailT 'unEmailT

emailTemplate :: (MonadRoute r m, Default r, MonadBrand m) => Html -> Html -> Html -> m Html
emailTemplate titleHtml leadHtml contentHtml = do
  indexLink <- routeToUrl def
  pn <- getProductName
  return $ H.docTypeHtml $ do
    H.head $ do
      H.style $ H.toHtml $ decodeUtf8 $(embedFile "email.css")
      H.title titleHtml
    H.body $ H.table $ do
      H.tr $ H.td $ H.table $ do
        H.tr $ H.td $ H.h1 titleHtml
        H.hr
        H.tr $ H.td $ H.p ! class_ "lead" $ leadHtml
        H.hr
        H.tr $ H.td $ contentHtml
      H.tr $ H.td $ H.table $ H.tr $ H.td $ do 
        H.hr
        H.p $ do
          "Brought to you by " 
          H.a ! A.href (fromString $ show indexLink) $ H.toHtml pn

tableSection :: Html -> [(Html, Html)] -> Html
tableSection titleText rows = do
  H.tr $ H.td $ H.h2 $ titleText
  forM_ rows $ \(title, content) -> H.tr $ do
    H.td title
    H.td content
