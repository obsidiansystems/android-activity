module Focus.Backend.Typeahead where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import Focus.Backend.Listen

typeaheadPatch :: (Ord k, Monoid a1)
               => NotificationType
               -> k -- Account Id
               -> t -- Account
               -> Set T.Text -- Queries
               -> ASetter a1 a a2 (Map T.Text (Maybe (Set k, Set k))) -- results lens
               -> ASetter a b a3 (Map k (Maybe t)) -- view lens
               -> (t -> Maybe T.Text) -- to text
               -> b -- patch
typeaheadPatch nt aid acc qs rlens vlens toText = 
  let results = Map.mapMaybe id $ Map.fromSet (\q -> isPrefix nt q aid acc toText) qs 
  in mempty & rlens .~ results
            & vlens .~ mapIntersectionWithSetPatchMap (Map.singleton aid (ntPatch nt acc)) results
  where
    mapIntersectionWithSetPatchMap :: (Ord k, Ord k') => Map k v -> Map k' (Maybe (Set k, Set k)) -> Map k v
    mapIntersectionWithSetPatchMap m spm = Map.intersection m $ Map.foldl (\xs mSetPatch ->
        case mSetPatch of
             Just (a, r) -> Map.fromSet (const ()) a <> Map.fromSet (const ()) r <> xs
             _ -> xs) Map.empty spm

typeaheadPatch' :: (Ord a, Monoid a2)
                => NotificationType
                -> a -- Account Id
                -> t -- Account
                -> Set T.Text -- Queries
                -> ASetter a2 a1 a3 (Map T.Text (Maybe (Set a, Set a))) -- Results lens
                -> ASetter a1 b a4 b1 -- view lens
                -> (t -> Maybe T.Text) -- to text
                -> (Map T.Text (Maybe (Set a, Set a)) -> b1) -- to view
                -> b -- patch
typeaheadPatch' nt aid acc qs rlens vlens toText f = let results = Map.mapMaybe id $ Map.fromSet (\q -> isPrefix nt q aid acc toText) qs in mempty
            & rlens .~ results
            & vlens .~ f results

isPrefix :: Ord a
         => NotificationType
         -> T.Text
         -> a
         -> t
         -> (t -> Maybe T.Text)
         -> Maybe (Maybe (Set a, Set a))
isPrefix nt q aid acc f = case fmap (q `T.isPrefixOf`) (f acc) of
  Just True -> Just $ Just $ ntSetPatch nt aid
  _ -> Nothing

ntPatch :: NotificationType -> a -> Maybe a
ntPatch nt' = case nt' of
  NotificationType_Delete -> const Nothing
  _ -> Just

ntSetPatch :: Ord a => NotificationType -> a -> (Set a, Set a)
ntSetPatch nt' = case nt' of
  NotificationType_Delete -> \x -> (mempty, Set.singleton x)
  _ -> \x -> (Set.singleton x, mempty)
