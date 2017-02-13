{-# LANGUAGE GADTs, LambdaCase #-}
module Focus.Backend.Typeahead where

import Control.Applicative
import Control.Lens
import Data.Foldable
import qualified Data.Map as DM
import Focus.AppendMap (AppendMap (..))
import qualified Focus.AppendMap as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Focus.Backend.Listen
import Focus.Patch

notificationToTypeaheadPatch :: (Ord k, Monoid s)
                             => NotificationType
                             -> k -- key
                             -> ElemPatch t -- value patch
                             -> (t -> Maybe Text)
                             -> (Patch t -> Maybe Text)
                             -> Set Text -- Queries
                             -> ASetter' s (Patch (AppendMap Text (Set k))) -- typeahead results
                             -> ASetter' s (Patch (AppendMap k t)) -- fetch results
                             -> s -- patch
notificationToTypeaheadPatch nt k x toTextValue toTextPatch qs rlens vlens =
  mempty & rlens .~ AppendMapPatch typeaheadPatch
         & vlens .~ AppendMapPatch fetchPatch
 where
  patchToIds = \case
    ElemPatch_Insert v -> Just v
    ElemPatch_Upsert _ (Just v) -> Just v
    _ -> Nothing
  typeaheadPatch = Map.mapMaybe id $ Map.fromSet (\q -> isPrefix nt q k x (elemPatchName toTextValue toTextPatch)) qs
  relevantFetches = fold
                  $ Map.mapMaybe patchToIds
                  $ typeaheadPatch
  fetchPatch = Map.intersection (Map.singleton k x) (Map.fromSet (\_ -> ()) relevantFetches)

--TODO: This produces patches that are larger than necessary when `NotificationType_Update`ing because we do not have access to the old version
isPrefix :: Ord a
         => NotificationType
         -> Text
         -> a
         -> t
         -> (t -> Maybe Text)
         -> Maybe (ElemPatch (Set a))
isPrefix nt q k x f = case fmap (q `T.isPrefixOf`) (f x) of
  Just True -> Just $ flip elemUpsert Set.empty $ SetPatch $ DM.singleton k $ case nt of
    NotificationType_Delete -> False
    _ -> True
  Just False -> case nt of
    NotificationType_Update -> Just $ flip elemUpsert Set.empty $ SetPatch $ DM.singleton k False
    _ -> Nothing
  _ -> Nothing

ntPatch :: NotificationType -> a -> ElemPatch a
ntPatch nt v = case nt of
  NotificationType_Delete -> ElemPatch_Remove
  _ -> ElemPatch_Insert v

elemPatchName :: (v -> Maybe Text) -> (Patch v -> Maybe Text) -> ElemPatch v -> Maybe Text
elemPatchName toTextValue toTextPatch = \case
  ElemPatch_Insert v -> toTextValue v
  ElemPatch_Upsert p mv -> toTextPatch p <|> (toTextValue =<< mv)
  ElemPatch_Remove -> Nothing

