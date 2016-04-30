{-# LANGUAGE TemplateHaskell, ConstraintKinds, RankNTypes, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses, DeriveFunctor, DeriveFoldable, DeriveTraversable, GADTs #-}
module Focus.Client.Types where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Catch
import Control.Monad.RWS.Strict
import Data.Aeson
import Data.Int
import Data.Map (Map)
import Network.WebSockets

import Focus.Account
import Focus.Api
import Focus.App
import Focus.Sign

newtype RequestId = RequestId { _unRequestId :: Int64 }
  deriving (Eq, Ord, Num, Show, Read, ToJSON, FromJSON)

newtype InterestId = InterestId { _unInterestId :: Int64 }
  deriving (Eq, Ord, Num, Show, Read, ToJSON, FromJSON)

newtype RequestM app a = RequestM { _runRequestM :: RWST (RequestEnv app) () (RequestState app) IO a }
  deriving (Functor, Applicative, Monad, MonadReader (RequestEnv app), MonadState (RequestState app), MonadIO, MonadMask, MonadCatch, MonadThrow)

data ClientConfig = ClientConfig
       { _clientConfig_host :: String
       , _clientConfig_port :: Int
       , _clientConfig_path :: String
       , _clientConfig_timeout :: Maybe Int
       }

data ClientEnv app = ClientEnv
       { _clientEnv_connection :: Connection
       , _clientEnv_nextRequestId :: TVar RequestId
       , _clientEnv_pendingRequests :: TVar (Map RequestId (TBQueue (Either String Value)))
       , _clientEnv_viewMap :: TVar (Map (Signed AuthToken) (View app))
       , _clientEnv_notifyViewChange :: TChan ()
       , _clientEnv_nextInterestId :: TVar InterestId
       , _clientEnv_interests :: TVar (Map InterestId (Signed AuthToken, ViewSelector app))
       }

data RequestEnv app = RequestEnv
       { _requestEnv_sendRequest :: forall rsp. (ToJSON rsp, FromJSON rsp)
                                 => (ApiRequest (PublicRequest app) (PrivateRequest app) rsp)
                                 -> IO (Async (Either String Value))
       , _requestEnv_registerInterest :: Signed AuthToken
                                      -> ViewSelector app
                                      -> IO (InterestId, STM ()) -- returns unregister action
       , _requestEnv_sendInterestSet :: IO ()
       , _requestEnv_listen :: forall a. Signed AuthToken
                            -> ViewSelector app
                            -> (View app -> Maybe a)
                            -> IO (Async a)
       }

data RequestState app = RequestState
       { _requestState_token :: Maybe (Signed AuthToken)
       , _requestState_timeout :: Maybe Int
       , _requestState_interests :: Map InterestId (ViewSelector app, STM ())
       }

data RequestResult rsp = RequestResult_Success rsp
                       | RequestResult_Failure String
                       | RequestResult_DecodeError Value String
                       | RequestResult_Timeout Int
                       | RequestResult_RequiresAuthorization
  deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance Monad RequestResult where
  return = RequestResult_Success
  x >>= f = case x of
    RequestResult_Success v -> f v
    RequestResult_Failure s -> RequestResult_Failure s
    RequestResult_DecodeError v s -> RequestResult_DecodeError v s
    RequestResult_Timeout n -> RequestResult_Timeout n
    RequestResult_RequiresAuthorization -> RequestResult_RequiresAuthorization

instance Applicative RequestResult where
  pure = return
  (<*>) = ap

data ListenResult a = ListenResult_Success a
                    | ListenResult_Failure String
                    | ListenResult_Timeout Int
                    | ListenResult_RequiresAuthorization
  deriving (Show, Read, Eq, Functor, Foldable, Traversable)

type MonadRequest app m = (MonadIO m, HasView app, HasRequest app, MonadReader (RequestEnv app) m, MonadState (RequestState app) m, MonadMask m)

makeWrapped ''RequestId
makeWrapped ''InterestId
makeLenses ''ClientEnv
makeLenses ''RequestEnv
makeLenses ''RequestState
makePrisms ''RequestResult
makePrisms ''ListenResult
