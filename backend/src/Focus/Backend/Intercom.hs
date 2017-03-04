{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Focus.Backend.Intercom
  ( module Focus.Backend.Intercom
  , module Focus.Intercom
  ) where

import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import qualified Data.Text.Encoding as T
import Network.HTTP.Conduit

import Focus.Intercom
import Focus.Intercom.Common

getIntercomUsers :: IntercomEnv -> IO [IntercomUser]
getIntercomUsers env = getIntercomUsers' Nothing []
  where
    url = "https://api.intercom.io/users/scroll"
    getReq env' mScrollParam = do
      man <- newManager tlsManagerSettings
      req <- fmap (asJson . applyBasicAuth user pass) $ parseUrlThrow $ maybe url ((url <> "?scroll_param=") <>) mScrollParam
      res <- try $ httpLbs req man
      case res of
        Left (err :: HttpException) -> do
          putStrLn $ "ERROR getIntercomUsers: " <> show err
          return mempty
        Right res' -> return $ responseBody res'
      where
        user = T.encodeUtf8 $ _intercomEnv_appId env'
        pass = T.encodeUtf8 $ unIntercomSecretKey $ _intercomEnv_secretKey env'
        asJson req = req { requestHeaders = ("Accept", "application/json"):requestHeaders req }

    getIntercomUsers' :: Maybe String -> [IntercomUser] -> IO [IntercomUser]
    getIntercomUsers' mScrollParam prev = do
      res <- getReq env mScrollParam
      case decode res of
        Nothing -> return prev
        Just body -> do
          let newMScrollParam = either error Just $ parseEither (body .:) "scroll_param"
          let users = either error id $ parseEither (body .:) "users"
          if null users then return prev else getIntercomUsers' newMScrollParam (map unIntercomUserInternal users <> prev)
