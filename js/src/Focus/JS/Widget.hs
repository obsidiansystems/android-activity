{-# LANGUAGE ScopedTypeVariables, RecursiveDo, ViewPatterns, TypeFamilies, FlexibleContexts, OverloadedStrings, TypeFamilies #-}
module Focus.JS.Widget where

import Control.Lens hiding (ix)
import Control.Monad
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Reflex.Dom

import Focus.App
import Focus.JS.App
import Focus.Patch

-- | refreshWidget w is a widget which acts like w, except it restarts whenever the Event that w produces is fired. This is useful for blanking forms on submit, for instance.
resetAfterEach :: (MonadWidget t m) => m (Event t a) -> m (Event t a)
resetAfterEach w = do rec success <- liftM (switch . current) $ widgetHold w (fmap (const $ w) success)
                      return success

data SpinnerParts m a b = SpinnerParts { _spinnerParts_pre  :: m ()
                                       , _spinnerParts_wait :: a -> m ()
                                       , _spinnerParts_post :: b -> m () }

spinner :: (MonadWidget t m) => SpinnerParts m a b -> Event t a -> Event t b -> m ()
spinner (SpinnerParts pre' wait post) request response = fmap (const ()) $ widgetHold pre' (leftmost [fmap post response, fmap wait request])

withSpinner :: MonadWidget t m => SpinnerParts m a b -> (Event t a -> m (Event t b)) -> (Event t a -> m (Event t b))
withSpinner sp asyncW request = do response <- asyncW request
                                   spinner sp request response
                                   return response

enumDropdown :: forall t m k. (MonadWidget t m, Enum k, Bounded k, Ord k) => (k -> Text) -> DropdownConfig t k -> m (Dropdown t k)
enumDropdown = enumDropdown' minBound

enumDropdown' :: forall t m k. (MonadWidget t m, Enum k, Bounded k, Ord k) => k -> (k -> Text) -> DropdownConfig t k -> m (Dropdown t k)
enumDropdown' d f cfg = do
  let xs = [minBound .. maxBound] :: [k]
      xMap = Map.fromList $ zip xs (map f xs)
  dropdown d (constDyn xMap) cfg

data ListEdit = InsertBefore | Delete | InsertAfter deriving (Eq, Ord, Show, Read)

extensibleListWidget :: forall t m a b. (MonadWidget t m)
                     => Int -- ^ Minimum number of entries (be careful: if this is 0, the entire list is allowed to vanish)
                     -> a -- ^ Initial entry for newly inserted items
                     -> [a] -- ^ Initial sequence of entries
                     -> (Dynamic t Int -> Dynamic t a -> m (Event t ListEdit, Dynamic t b)) -- ^ Widget for a single item which is expected to include
                                                                                            -- the list editing controls and pass through the resulting events.
                     -> m (Dynamic t [b])
extensibleListWidget n x0 xs0 itemWidget =
  let genIndex :: Map Rational a -> Map Rational a -> Rational
      genIndex us vs =
        case (Map.maxViewWithKey us, Map.minViewWithKey vs) of
          (Nothing       , Nothing       ) -> 0
          (Nothing       , Just ((v,_),_)) -> v - 1
          (Just ((u,_),_), Nothing       ) -> u + 1
          (Just ((u,_),_), Just ((v,_),_)) -> (u + v) / 2
      handleChange :: (Rational, ListEdit) -> Map Rational a -> Map Rational a
      handleChange (k,InsertBefore) xs = let (us, x, vs) = Map.splitLookup k xs
                                             vs' = Map.alter (const x) k vs
                                             i = genIndex us vs'
                                          in Map.insert i x0 (Map.union us vs')
      handleChange (k,Delete)       xs = if Map.size xs > n then Map.delete k xs
                                                            else Map.insert k x0 xs
      handleChange (k,InsertAfter)  xs = let (us, x, vs) = Map.splitLookup k xs
                                             us' = Map.alter (const x) k us
                                             i = genIndex us' vs
                                          in Map.insert i x0 (Map.union us' vs)
      map0 = Map.fromList . zip [0..] $ xs0
  in do rec listMapD <- foldDyn (\m xs -> foldr handleChange xs (Map.toList m)) map0 changeMapE
            ixMapD <- mapDyn (Map.fromList . (`zip` [0..]) . Map.keys) listMapD
            resultMapD <- listWithKey listMapD $ \k vD -> do
                            ix <- mapDyn (Map.findWithDefault (-1) k) ixMapD
                                    -- TODO, maybe: figure out why this Map lookup is too strict.
                                    -- Deleting an item causes a failed lookup, however, I'm not sure it really matters.
                            itemWidget ix vD
            changeMapE <- fmap (switch . current) . mapDyn (mergeMap . fmap fst) $ resultMapD
            valuesMapD <- fmap joinDynThroughMap . mapDyn (fmap snd) $ resultMapD
            valuesD <- mapDyn Map.elems valuesMapD
        return valuesD

typeaheadSearch :: (MonadFocusWidget app t m, Monoid a)
                => Text
                -- ^ text input placeholder
                -> ASetter a (ViewSelector app) b (Set Text)
                -- ^ setter for query field of view selector
                -> (View app -> s)
                -- ^ extractor for relevant things from the view
                -> m (Dynamic t s, Dynamic t Text)
                -- ^ results and the search string
typeaheadSearch ph vsQuery extractor = do
  search <- textInput $ def & attributes .~ (constDyn $ "placeholder" =: ph)
  aspenView <- watchViewSelectorLensSet vsQuery $ value search
  result <- mapDyn extractor aspenView
  return (result, value search)

typeaheadSearchDropdown :: (MonadFocusWidget app t m, Ord k, Monoid a)
                        => Text
                        -- ^ text input placeholder
                        -> ASetter a (ViewSelector app) b (Set Text)
                        -- ^ setter for query field of view selector
                        -> (View app -> s)
                        -- ^ extractor for relevant things from the view
                        -> (s -> Map k Text)
                        -- ^ convert relevant things into a Map for dropdown
                        -> m (Dynamic t (Maybe k))
typeaheadSearchDropdown ph vsQuery extractor toStringMap = do
  (xs, _) <- typeaheadSearch ph vsQuery extractor
  options <- forDyn xs $ \xMap -> Nothing =: "" <> Map.mapKeysMonotonic Just (toStringMap xMap)
  fmap value $ dropdown Nothing options def

typeaheadSearchMultiselect :: (MonadFocusWidget app t m, Ord k, Monoid a)
                           => Text
                           -- ^ text input placeholder
                           -> ASetter a (ViewSelector app) b (Set Text)
                           -- ^ setter for query field of view selector
                           -> (View app -> s)
                           -- ^ extractor for relevant things from the view
                           -> (s -> Map k (m ()))
                           -- ^ convert relevant things into a Map for listing
                           -> Dynamic t (Set k)
                           -- ^ values that are already selected
                           -> m (Dynamic t (SetPatch k))
typeaheadSearchMultiselect ph vsQuery extractor toWidgetMap selections0 = do
  (xs, _) <- typeaheadSearch ph vsQuery extractor
  options <- mapDyn toWidgetMap xs
  diffListWithKey options selections0

diffListWithKey :: (Ord k, MonadWidget t m)
                => Dynamic t (Map k (m ()))
                -> Dynamic t (Set k)
                -> m (Dynamic t (SetPatch k))
diffListWithKey ms selDyn = do
  selects <- fmap joinDynThroughMap . el "ul" . listWithKey ms $ \k w -> el "li" $ do
    pb <- getPostBuild -- awful hack? These widgets are all rebuilt when I save the bundle, unexpectedly.
    include <- checkbox False $ def & attributes .~ (constDyn mempty)
                                    & setValue .~ fmap (Set.member k) (leftmost [updated selDyn, tagDyn selDyn pb])
    void $ dyn w
    return $ value include
  mapDyn SetPatch selects
