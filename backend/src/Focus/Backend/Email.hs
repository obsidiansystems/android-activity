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

class Monad m => MonadEmail m where
  sendMail :: Mail -> m ()

type EmailEnv = (HostName, PortNumber, UserName, Password)

instance FromJSON PortNumber where
  parseJSON v = do
    n :: Word16 <- parseJSON v
    return $ fromIntegral n

instance ToJSON PortNumber where
  toJSON n = toJSON (fromIntegral n :: Word16)

newtype EmailT m a = EmailT { unEmailT :: ReaderT EmailEnv m a } deriving (Functor, Applicative, Monad, MonadIO, MonadRoute, MonadSign, MonadBrand)

instance MonadIO m => MonadEmail (EmailT m) where
  sendMail mail = do
    (server, port, username, password) <- EmailT ask
    liftIO $ putStrLn $ "Sending email " <> show (map snd $ filter ((=="Subject") . fst) $ mailHeaders mail) <> " to " <> show (map snd $ filter ((=="To") . fst) $ mailHeaders mail)
    liftIO $ sendMailWithLogin' server port username password mail

instance MonadEmail m => MonadEmail (ReaderT r m) where
  sendMail = lift . sendMail

runEmailT = runReaderT . unEmailT

sendEmailFrom :: MonadEmail m => Text -> Text -> NonEmpty Text -> Text -> Html -> m ()
sendEmailFrom name email recipients subject body = sendMail $ simpleMail (Address (Just name) email) (map (Address Nothing) $ toList recipients) [] [] subject [(htmlPart $ renderHtml body)]

deriveNewtypePersistBackend (\m -> [t| EmailT $m |]) (\m -> [t| ReaderT EmailEnv $m |]) 'EmailT 'unEmailT


