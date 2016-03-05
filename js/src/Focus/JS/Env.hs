{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, RecursiveDo, FlexibleContexts #-}
module Focus.JS.Env where

import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock
import Focus.Request
import Focus.Schema
import Network.HTTP.Types.URI
import Reflex.Dom hiding (webSocket)
import Focus.JS.WebSocket

listenNotifications :: forall t x m notification authToken. (MonadFix m, MonadWidget t m, HasJS x m, HasJS x (WidgetHost m), FromJSON notification, ToJSON authToken)
                    => Maybe authToken
                    -> m (Event t [notification])
listenNotifications auth = do
  eNotifications' <- liftM (fmapMaybe (decodeValue' . LBS.fromStrict) . _webSocket_recv) $
    webSocket ("/listen?token=" <> (T.unpack . decodeUtf8 . urlEncode True . LBS.toStrict . encode $ auth)) def
  rec notificationBuffer <- foldDyn (\a b -> maybe [] (\a' -> reverse a' ++ b) a) [] $ leftmost [fmap Just eNotifications', fmap (const Nothing) eNotifications]
      t <- tickLossy 1 =<< liftIO getCurrentTime
      let eNotifications = ffilter (not . null) . fmap reverse $ tag (current notificationBuffer) t
  return eNotifications

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

