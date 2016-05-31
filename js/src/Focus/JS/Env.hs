{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, RecursiveDo, FlexibleContexts, TypeFamilies, OverloadedStrings #-}
module Focus.JS.Env where

import Control.Lens
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import Focus.AppendMap
import Focus.Request
import Focus.Schema
import Focus.WebSocket
import Reflex.Dom hiding (webSocket, Value)
import Data.Text (Text)

import Focus.JS.WebSocket

openAndListenWebsocket :: forall t m x token notification req rsp vs. (MonadWidget t m, HasJS x m, FromJSON notification, FromJSON rsp, ToJSON token, ToJSON req, ToJSON vs, Ord token, FromJSON token)
                       => Event t [(Value, req)]
                       -> Event t (AppendMap token vs)
                       -> m (Event t (AppendMap token notification), Event t (Value, Either Text rsp))
openAndListenWebsocket eReq eViewSelectorWithAuth = do
  (eMessages :: Event t (Either Text (WebSocketData token notification (Either Text rs)))) <- liftM (fmapMaybe (decodeValue' . LBS.fromStrict) . _webSocket_recv) $
    webSocket ("/listen")
      (WebSocketConfig $ fmap (map (LBS.toStrict . encode)) $ mconcat [ fmap (map (uncurry WebSocketData_Api)) eReq
                                                                      , fmap ((:[]) . WebSocketData_Listen) eViewSelectorWithAuth
                                                                      ])
  --TODO: Handle parse errors returned by the backend
  let eNotifications = fmapMaybe (^? _Right . _WebSocketData_Listen) eMessages
      eResponses = fmapMaybe (^? _Right . _WebSocketData_Api) eMessages
  return (eNotifications, eResponses)

class (Ord (Id a)) => EnvType e a a' | a -> a', a' -> a where
  allInEnv :: (MonadReader (e t) m) => m (Dynamic t (Map (Id a) a'))

-- | Get a dynamic map of all the things in the environment satisfying the given condition.
filterEnv :: (MonadWidget t m, MonadReader (e t) m, EnvType e a a') => (a' -> Bool) -> m (Dynamic t (Map (Id a) a'))
filterEnv p = do mD <- allInEnv
                 mapDyn (Map.filter p) mD

filterWithKeyEnv :: (MonadWidget t m, MonadReader (e t) m, EnvType e a a') => (Id a -> a' -> Bool) -> m (Dynamic t (Map (Id a) a'))
filterWithKeyEnv p = do mD <- allInEnv
                        mapDyn (Map.filterWithKey p) mD

-- | Look up the given Id in the appropriate Map in the environment.
lookupEnv :: (MonadWidget t m, MonadReader (e t) m, EnvType e a a') => Id a -> m (Dynamic t (Maybe a'))
lookupEnv aid = do mD <- allInEnv
                   mapDyn (Map.lookup aid) mD

-- | Given a function which extracts an Id from a record, and a dynamic Map of records, look up the thing associated with the Id in the environment, pairing each record with the result of the lookup.
adjoinEnv :: (MonadWidget t m, MonadReader (e t) m, EnvType e b b') => (a -> Id b) -> Dynamic t (Map x a) -> m (Dynamic t (Map x (a, b')))
adjoinEnv f aMapD = do bMapD <- allInEnv
                       combineDyn (\aMap bMap -> Map.mapMaybe (\a -> do b <- Map.lookup (f a) bMap; return (a,b)) 
                                                              aMap)
                                  aMapD bMapD

