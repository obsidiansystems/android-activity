{-# LANGUAGE TemplateHaskell, ConstraintKinds, RankNTypes, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Focus.Client.Types where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Catch
import Control.Monad.RWS.Strict
import Data.Aeson
import Data.Int
import Data.Map (Map)
import Data.Semigroup
import Network.WebSockets

import Focus.Account
import Focus.Api
import Focus.Sign

newtype RequestId = RequestId { _unRequestId :: Int64 }
  deriving (Eq, Ord, Num, Show, Read, ToJSON, FromJSON)

newtype InterestId = InterestId { _unInterestId :: Int64 }
  deriving (Eq, Ord, Num, Show, Read, ToJSON, FromJSON)

newtype RequestM pub priv select view a = RequestM { _runRequestM :: RWST (RequestEnv pub priv select view) () (RequestState select) IO a }
  deriving (Functor, Applicative, Monad, MonadReader (RequestEnv pub priv select view), MonadState (RequestState select), MonadIO, MonadMask, MonadCatch, MonadThrow)

data ClientConfig select view patch = ClientConfig
       { _clientConfig_host :: String
       , _clientConfig_port :: Int
       , _clientConfig_path :: String
       , _clientConfig_emptyView :: view
       , _clientConfig_patchView :: (patch -> view -> view)
       , _clientConfig_cropView :: (select -> view -> view)
       , _clientConfig_timeout :: Maybe Int
       }

data ClientEnv select view patch = ClientEnv
       { _clientEnv_connection :: Connection
       , _clientEnv_nextRequestId :: TVar RequestId
       , _clientEnv_pendingRequests :: TVar (Map RequestId (TBQueue (Either String Value)))
       , _clientEnv_emptyView :: view
       , _clientEnv_viewMap :: TVar (Map (Signed AuthToken) view)
       , _clientEnv_patchView :: (patch -> view -> view)
       , _clientEnv_cropView :: (select -> view -> view)
       , _clientEnv_notifyViewChange :: TChan ()
       , _clientEnv_nextInterestId :: TVar InterestId
       , _clientEnv_interests :: TVar (Map InterestId (Signed AuthToken, select))
       }

data RequestEnv pub priv select view = RequestEnv
       { _requestEnv_sendRequest :: forall rsp. (ToJSON rsp, FromJSON rsp) => (ApiRequest pub priv rsp) -> IO (Async (Either String Value))
       , _requestEnv_registerInterest :: Signed AuthToken -> select -> IO (InterestId, STM ()) -- returns unregister action
       , _requestEnv_sendInterestSet :: (ToJSON select, Monoid select) => IO ()
       , _requestEnv_listen :: forall a. Signed AuthToken -> select -> (view -> Maybe a) -> IO (Async a)
       }

data RequestState select = RequestState
       { _requestState_token :: Maybe (Signed AuthToken)
       , _requestState_timeout :: Maybe Int
       , _requestState_interests :: Map InterestId (select, STM ())
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

type MonadRequest pub priv select view m = (MonadIO m, MonadReader (RequestEnv pub priv select view) m, MonadState (RequestState select) m, MonadMask m, Semigroup select, Monoid select, ToJSON select)

makeWrapped ''RequestId
makeWrapped ''InterestId
makeLenses ''ClientEnv
makeLenses ''RequestEnv
makeLenses ''RequestState
makePrisms ''RequestResult
makePrisms ''ListenResult
