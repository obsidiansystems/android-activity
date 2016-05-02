{-# LANGUAGE ScopedTypeVariables, RecursiveDo, ViewPatterns #-}
module Focus.JS.Widget where

import Control.Lens hiding (ix)
import Control.Monad
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Focus.App
import Focus.JS.App
import Reflex.Dom
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

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

enumDropdown :: forall t m k. (MonadWidget t m, Enum k, Bounded k, Show k, Read k, Ord k) => (k -> String) -> DropdownConfig t k -> m (Dropdown t k)
enumDropdown = enumDropdown' minBound 

enumDropdown' :: forall t m k. (MonadWidget t m, Enum k, Bounded k, Show k, Read k, Ord k) => k -> (k -> String) -> DropdownConfig t k -> m (Dropdown t k)
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
                => String
                -- ^ text input placeholder
                -> ASetter a (ViewSelector app) b (Set Text)
                -- ^ setter for query field of view selector
                -> (View app -> s)
                -- ^ extractor for relevant things from the view
                -> m (Dynamic t s)
typeaheadSearch placeholder vsQuery extractor = do
  search <- textInput $ def & attributes .~ (constDyn $ "placeholder" =: placeholder)
  aspenView <- watchViewSelectorLensSet vsQuery =<< mapDyn T.pack (value search)
  mapDyn extractor aspenView

typeaheadSearchDropdown :: (MonadFocusWidget app t m, Ord k, Read k, Show k, Monoid a)
                        => String
                        -- ^ text input placeholder
                        -> ASetter a (ViewSelector app) b (Set Text)
                        -- ^ setter for query field of view selector
                        -> (View app -> s)
                        -- ^ extractor for relevant things from the view
                        -> (s -> Map k String)
                        -- ^ convert relevant things into a Map for dropdown
                        -> m (Dynamic t (Maybe k))
typeaheadSearchDropdown placeholder vsQuery extractor toStringMap = do
  xs <- typeaheadSearch placeholder vsQuery extractor
  options <- forDyn xs $ \xMap -> Nothing =: "" <> Map.mapKeysMonotonic Just (toStringMap xMap)
  fmap value $ dropdown Nothing options def

typeaheadSearchMultiselect :: (MonadFocusWidget app t m, Ord k, Read k, Show k, Monoid a)
                           => String
                           -- ^ text input placeholder
                           -> ASetter a (ViewSelector app) b (Set Text)
                           -- ^ setter for query field of view selector
                           -> (View app -> s)
                           -- ^ extractor for relevant things from the view
                           -> (s -> Map k (m ()))
                           -- ^ convert relevant things into a Map for listing
                           -> Set k
                           -> m (Dynamic t (Set k))
typeaheadSearchMultiselect ph vsQuery extractor toWidgetMap selections0 = do
  xs <- typeaheadSearch ph vsQuery extractor
  options <- mapDyn toWidgetMap xs
  selections <- fmap joinDynThroughMap $ el "ul" $ listWithKey options $ \k w -> el "li" $ do
    cb <- checkbox (Set.member k selections0) def
    dyn w
    return $ value cb
  mapDyn (Map.keysSet . Map.filter id) selections

