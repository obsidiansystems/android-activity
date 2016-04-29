module Focus.Backend.Typeahead where

import Control.Lens
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T

import Focus.Backend.Listen
import Focus.Patch

notificationToTypeaheadPatch :: (Ord k, Monoid s)
                             => NotificationType
                             -> k -- key
                             -> t -- value
                             -> (t -> Maybe Text)
                             -> Set Text -- Queries
                             -> ASetter' s (Map Text (Maybe (SetPatch k))) -- typeahead results
                             -> ASetter' s (Map k (Maybe t)) -- fetch results
                             -> s -- patch
notificationToTypeaheadPatch nt k x toText qs rlens vlens =
  mempty & rlens .~ typeaheadPatch
         & vlens .~ fetchPatch
 where
  typeaheadPatch = Map.mapMaybe id $ Map.fromSet (\q -> isPrefix nt q k x toText) qs
  relevantFetches = fold $ fmap (Map.keysSet . Map.filter id . view _Wrapped) $ Map.mapMaybe id $ typeaheadPatch
  fetchPatch = Map.intersection (Map.singleton k (ntPatch nt x)) (Map.fromSet (\_ -> ()) relevantFetches)

--TODO: This produces patches that are larger than necessary when `NotificationType_Update`ing because we do not have access to the old version
isPrefix :: Ord a
         => NotificationType
         -> Text
         -> a
         -> t
         -> (t -> Maybe Text)
         -> Maybe (Maybe (SetPatch a))
isPrefix nt q k x f = case fmap (q `T.isPrefixOf`) (f x) of
  Just True -> Just $ Just $ SetPatch $ Map.singleton k $ case nt of
    NotificationType_Delete -> False
    _ -> True
  Just False -> case nt of
    NotificationType_Update -> Just $ Just $ SetPatch $ Map.singleton k False
    _ -> Nothing
  _ -> Nothing

ntPatch :: NotificationType -> a -> Maybe a
ntPatch nt = case nt of
  NotificationType_Delete -> const Nothing
  _ -> Just
