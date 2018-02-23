{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Focus.JS.Widget
  ( module Focus.JS.Widget
  , improvingMaybe
  ) where

import Control.Lens hiding (ix, element)
import Control.Monad
import Control.Monad.Fix
import Data.AppendMap (AppendMap)
import qualified Data.AppendMap as AMap
import qualified Data.Dependent.Map as DMap
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dynamic (improvingMaybe)
import Reflex.Dom.Core hiding (Delete)

import Focus.App
import Focus.Highlight
import Focus.JS.App
import Focus.JS.FontAwesome
import Focus.JS.Highlight
import Focus.Patch

import qualified GHCJS.DOM.HTMLElement as E
import qualified GHCJS.DOM.HTMLInputElement as IE

-- | A variant of elDynAttr which sets the element's style to "display: none;" initially, until its attributes can be set properly.
elDynAttrHide :: forall t m a. (DomBuilder t m, PostBuild t m) => Text -> Dynamic t (Map Text Text) -> m a -> m a
elDynAttrHide elementTag attrs child = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let cfg = def
        & initialAttributes .~ "style" =: "display: none;"
        & elementConfig_namespace .~ Nothing
        & modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
  snd <$> element elementTag cfg child

-- | refreshWidget w is a widget which acts like w, except it restarts whenever the Event that w produces is fired. This is useful for blanking forms on submit, for instance.
resetAfterEach :: (DomBuilder t m, MonadHold t m, MonadFix m) => m (Event t a) -> m (Event t a)
resetAfterEach w = do rec success <- liftM (switch . current) $ widgetHold w (fmap (const $ w) success)
                      return success

data SpinnerParts m a b = SpinnerParts { _spinnerParts_pre  :: m ()
                                       , _spinnerParts_wait :: a -> m ()
                                       , _spinnerParts_post :: b -> m () }

spinner :: (DomBuilder t m, MonadHold t m) => SpinnerParts m a b -> Event t a -> Event t b -> m ()
spinner (SpinnerParts pre' wait post) request response = fmap (const ()) $ widgetHold pre' (leftmost [fmap post response, fmap wait request])

withSpinner :: (DomBuilder t m, MonadHold t m) => SpinnerParts m a b -> (Event t a -> m (Event t b)) -> (Event t a -> m (Event t b))
withSpinner sp asyncW request = do response <- asyncW request
                                   spinner sp request response
                                   return response

enumDropdown :: forall t m k. (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, Ord k, Enum k, Bounded k) => (k -> Text) -> DropdownConfig t k -> m (Dropdown t k)
enumDropdown = enumDropdown' minBound

enumDropdown' :: forall t m k. (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, Ord k, Enum k, Bounded k) => k -> (k -> Text) -> DropdownConfig t k -> m (Dropdown t k)
enumDropdown' d f cfg = do
  let xs = [minBound .. maxBound] :: [k]
      xMap = Map.fromList $ zip xs (map f xs)
  dropdown d (constDyn xMap) cfg

data ListEdit = InsertBefore | Delete | InsertAfter deriving (Eq, Ord, Show, Read)

extensibleListWidget :: forall t m a b. (MonadWidget t m)
                     => Int -- ^ Minimum number of entries (be careful: if this is 0, the entire list is allowed to vanish)
                     -> a -- ^ Initial entry for newly inserted items
                     -> [a] -- ^ Initial sequence of entries
                     -> (Dynamic t Int -> a -> m (Event t ListEdit, Dynamic t b))
                     -- ^ Widget for a single item which is expected to include
                     -- the list editing controls and pass through the resulting events.
                     -> m (Dynamic t [b])
extensibleListWidget n x0 xs0 itemWidget = extensibleListWidgetWithSize n x0 xs0 (\d -> itemWidget (fst <$> d))

-- | Like `extensibleListWidget`, but the items know the current size of the whole list, as well as their position.
extensibleListWidgetWithSize :: forall t m a b. (MonadWidget t m)
                             => Int -- ^ Minimum number of entries (be careful: if this is 0, the entire list is allowed to vanish)
                             -> a -- ^ Initial entry for newly inserted items
                             -> [a] -- ^ Initial sequence of entries
                             -> (Dynamic t (Int, Int) -> a -> m (Event t ListEdit, Dynamic t b))
                             -- ^ Widget for a single item which is expected to include
                             -- the list editing controls and pass through the resulting events.
                             -- First argument is (item position, total number of items).
                             -> m (Dynamic t [b])
extensibleListWidgetWithSize n x0 xs0 itemWidget = do
  let genIndex :: Map Rational a -> Map Rational a -> Rational
      genIndex us vs =
        case (Map.maxViewWithKey us, Map.minViewWithKey vs) of
          (Nothing       , Nothing       ) -> 0
          (Nothing       , Just ((v,_),_)) -> v - 1
          (Just ((u,_),_), Nothing       ) -> u + 1
          (Just ((u,_),_), Just ((v,_),_)) -> (u + v) / 2
      handleChange :: (Rational, ListEdit) -> Map Rational a -> Map Rational (Maybe a)
      handleChange (k,InsertBefore) xs = let (us, x, vs) = Map.splitLookup k xs
                                             vs' = Map.alter (const x) k vs
                                             i = genIndex us vs'
                                          in Map.singleton i (Just x0)
      handleChange (k,Delete)       xs = if Map.size xs > n then Map.singleton k Nothing
                                                            else Map.singleton k (Just x0)
      handleChange (k,InsertAfter)  xs = let (us, x, vs) = Map.splitLookup k xs
                                             us' = Map.alter (const x) k us
                                             i = genIndex us' vs
                                          in Map.singleton i (Just x0)
      map0 :: Map Rational (Maybe a) = Map.fromList . zip [0..] $ fmap Just xs0
  rec let updateEvent :: Event t (Map Rational (Maybe a)) = attachWith (\xs x -> handleChange x xs) (current listMapD) changeMapE
      listMapD :: Dynamic t (Map Rational a) <- fmap (Map.mapMaybe id) <$> foldDyn (\e m -> Map.union e m) map0 updateEvent
      let ixMapD :: Dynamic t (Map Rational Int) = fmap (Map.fromList . (`zip` [0::Int ..]) . Map.keys) listMapD
      resultMapD <- listHoldWithKey (Map.mapMaybe id map0) updateEvent $ \k v -> do
        let ix = fmap (Map.findWithDefault (-1) k) ixMapD
                -- TODO, maybe: figure out why this Map lookup is too strict.
                -- Deleting an item causes a failed lookup, however, I'm not sure it really matters.
        itemWidget ((,) <$> ix <*> fmap length listMapD) v
      let changeMapE = switch . current $ fmap (leftmost . fmap (\(k, v) -> fmap ((,) k) v) . Map.toList . fmap fst) $ resultMapD
          valuesMapD = joinDynThroughMap $ fmap (fmap snd) $ resultMapD
          valuesD = fmap Map.elems valuesMapD
  return valuesD

typeaheadSearch :: (MonadFocusWidget app t m)
                => Text
                -- ^ text input placeholder
                -> (Text -> ViewSelector app SelectedCount)
                -- ^ setter for query field of view selector
                -> (View app -> s)
                -- ^ extractor for relevant things from the view
                -> m (Dynamic t s, Dynamic t Text)
                -- ^ results and the search string
typeaheadSearch ph vsQuery extractor = do
  search <- inputElement $ def & initialAttributes .~ "placeholder" =: ph
  aspenView <- watchViewSelector $ vsQuery <$> value search
  let result = fmap extractor aspenView
  return (result, value search)

typeaheadSearchDropdown :: (MonadFocusWidget app t m, Ord k)
                        => Text
                        -- ^ text input placeholder
                        -> (Text -> ViewSelector app SelectedCount)
                        -- ^ setter for query field of view selector
                        -> (View app -> s)
                        -- ^ extractor for relevant things from the view
                        -> (s -> Map k Text)
                        -- ^ convert relevant things into a Map for dropdown
                        -> m (Dynamic t (Maybe k))
typeaheadSearchDropdown ph vsQuery extractor toStringMap = do
  (xs, _) <- typeaheadSearch ph vsQuery extractor
  let options = ffor xs $ \xMap -> Nothing =: "" <> Map.mapKeysMonotonic Just (toStringMap xMap)
  fmap value $ dropdown Nothing options def

typeaheadSearchMultiselect :: (MonadFocusWidget app t m, Ord k)
                           => Text
                           -- ^ text input placeholder
                           -> (Text -> ViewSelector app SelectedCount)
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
  let options = fmap toWidgetMap xs
  diffListWithKey options selections0

diffListWithKey :: (Ord k, MonadWidget' t m)
                => Dynamic t (Map k (m ()))
                -> Dynamic t (Set k)
                -> m (Dynamic t (SetPatch k))
diffListWithKey ms selDyn = do
  selects <- fmap joinDynThroughMap . el "ul" . listWithKey ms $ \k w -> el "li" $ do
    pb <- getPostBuild
    include <- checkbox False $ def & attributes .~ (constDyn mempty)
                                    & setValue .~ fmap (Set.member k) (leftmost [updated selDyn, tagPromptlyDyn selDyn pb])
    void $ dyn w
    return $ value include
  return $ fmap SetPatch selects

data ComboBoxAction = ComboBoxAction_Up
                    | ComboBoxAction_Down
                    | ComboBoxAction_Hover
                    | ComboBoxAction_Select
                    | ComboBoxAction_Dismiss
                    | ComboBoxAction_Backspace
                    | ComboBoxAction_Left
                    | ComboBoxAction_Right
  deriving (Show, Read, Eq, Ord)

{-# DEPRECATED keycodeUp "Use 'ArrowUp' from the keycode package instead" #-}
keycodeUp :: Int
keycodeUp = 38

{-# DEPRECATED keycodeLeft "Use 'ArrowLeft' from the keycode package instead" #-}
keycodeLeft :: Int
keycodeLeft = 37

{-# DEPRECATED keycodeRight "Use 'ArrowRight' from the keycode package instead" #-}
keycodeRight :: Int
keycodeRight = 39

{-# DEPRECATED keycodeDown "Use 'ArrowDown' from the keycode package instead" #-}
keycodeDown :: Int
keycodeDown = 40

{-# DEPRECATED keycodeBackspace "Use 'Backspace' from the keycode package instead" #-}
keycodeBackspace :: Int
keycodeBackspace = 8

comboBoxInput :: forall t m. (DomBuilder t m)
              => InputElementConfig EventResult t (DomBuilderSpace m)
              -> m (InputElement EventResult (DomBuilderSpace m) t, Event t ComboBoxAction)
comboBoxInput cfg = do
  let keydownFlags = \case
        Just (EventResult k) -> case keyCodeLookup $ fromIntegral k of
          ArrowUp -> preventDefault
          ArrowDown -> preventDefault
          _ -> mempty
        Nothing -> mempty
  t <- inputElement $ cfg
    & inputElementConfig_elementConfig . elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Keydown keydownFlags
  let keypressAction = ComboBoxAction_Select <$ ffilter ((== Enter) . keyCodeLookup . fromIntegral) (domEvent Keypress t)
      keydownAction = fforMaybe (domEvent Keydown t) $ \k -> case keyCodeLookup $ fromIntegral k of
        Escape -> Just ComboBoxAction_Dismiss
        Backspace -> Just ComboBoxAction_Backspace
        ArrowLeft -> Just ComboBoxAction_Left
        ArrowRight -> Just ComboBoxAction_Right
        ArrowUp -> Just ComboBoxAction_Up
        ArrowDown -> Just ComboBoxAction_Down
        _ -> Nothing
  return (t, leftmost [keypressAction, keydownAction])

comboBoxList :: (Ord k, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
             => Dynamic t (Map k v)
             -> (k -> Dynamic t v -> Dynamic t Bool -> Dynamic t Text -> m (Event t ComboBoxAction)) -- ^ Returns child element hover event
             -> Dynamic t Text -- ^ Query
             -> Event t ComboBoxAction
             -> m (Event t k)
comboBoxList xs li query externalActions = do
  let xs' = fmap (Map.mapKeysMonotonic Just) xs
  rec focusE <- selectViewListWithKey focus xs' (\k v isFocused -> maybe (return never) (\k' -> li k' v isFocused query) k)
      let actions = leftmost [ attachWith (\xs'' (k, a) -> ((k, xs''), a)) (current xs') focusE
                             , attach ((,) <$> current focus <*> current xs') externalActions
                             ]
          actionToFocus = fforMaybe actions $ \((k, vs), action) -> case action of
            ComboBoxAction_Up -> fmap fst $ Map.lookupLT k vs
            ComboBoxAction_Down -> fmap fst $ Map.lookupGT k vs
            ComboBoxAction_Hover -> Just k
            _ -> Nothing
          mapMin = listToMaybe . Map.keys
          selectOnUpdate = attachWithMaybe (\f es -> if Map.member f es then Just f else mapMin es) (current focus) $ updated xs'
      focus <- foldDyn (\a _ -> a) Nothing $ leftmost
        [ Nothing <$ ffilter Map.null (updated xs)
        , actionToFocus
        , selectOnUpdate
        ]
  return $ fmapMaybe id $ tag (current focus) $ ffilter ((==ComboBoxAction_Select) . snd) actions

comboBox :: (Ord k, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
         => InputElementConfig EventResult t (DomBuilderSpace m)
         -> (Dynamic t Text -> m (Dynamic t (Map k v)))
         -> (k -> Dynamic t v -> Dynamic t Bool -> Dynamic t Text -> m (Event t ComboBoxAction))
         -> (k -> v -> Text)
         -> (forall a. m a -> m a)
         -> m (Event t k)
comboBox cfg getOptions li toStr wrapper = do
  rec (t, inputActions) <- comboBoxInput $ cfg & inputElementConfig_setValue .~ leftmost [fromMaybe never $ _inputElementConfig_setValue cfg, selectionString]
      userInput <- holdDyn "" $ leftmost [ _inputElement_input t
                                         , "" <$ selectionE
                                         ]
      options <- getOptions userInput
      selectionE <- wrapper $ comboBoxList options li (_inputElement_value t) inputActions
      let selectionString = attachWith (\xs k -> maybe "" (toStr k) $ Map.lookup k xs) (current options) selectionE
  return selectionE

simpleCombobox :: forall app t m k v. (HasView app, MonadFocusWidget app t m, Ord k)
               => (Text -> ViewSelector app SelectedCount) -- ^ Convert query to ViewSelector
               -> (View app -> Map k v) -- ^ Get a map of results from the resulting View
               -> (k -> v -> Text) -- ^ Turn a result into a string for display
               -> (Text -> Text -> HighlightedText) -- ^ Highlight results
               -> m (Event t k) -- ^ Selection event
simpleCombobox toVS fromView toString highlighter = elClass "span" "simple-combobox" $ do
  rec let getOptions :: Dynamic t Text -> m (Dynamic t (Map k v))
          getOptions q = do
            vs <- foldDyn (\a _ -> case a of Nothing -> mempty; Just a' -> toVS a') mempty $
              leftmost [ Just <$> updated q, Nothing <$ selection ]
            v <- watchViewSelector vs
            return $ fmap fromView v
      selection <- comboBox def getOptions (comboBoxListItem highlighter toString) toString (el "ul")
  return selection

comboBoxListItem :: (DomBuilder t m, PostBuild t m, MonadHold t m)
                 => (Text -> Text -> HighlightedText) -- ^ Highlight results (Query -> Result Text -> Highlight)
                 -> (k -> v -> Text) -- ^ Turn a result into a string for display
                 -> k
                 -> Dynamic t v
                 -> Dynamic t Bool
                 -> Dynamic t Text
                 -> m (Event t ComboBoxAction)
comboBoxListItem highlighter toString k v sel q = do
  let selAttr = fmap (\x -> "class" =: if x then "simple-combobox-focus" else "simple-combobox-blur") sel
  (e, _) <- elDynAttr' "li" selAttr $ dynHighlightedTextQ highlighter q $ fmap (toString k) v
  return $ leftmost [ ComboBoxAction_Hover <$ domEvent Mouseover e
                    , ComboBoxAction_Select <$ domEvent Click e
                    ]

-- | Whenever the header is clicked, it toggles the "collapsed" state of the
-- content, making it "display: none" and hiding it entirely.
collapsibleSection :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Text -> m a -> m a
collapsibleSection = collapsibleSectionWithDefault True

-- | Like `collapsibleSection` but takes a Bool to indicate if sections should be collapsed by default
collapsibleSectionWithDefault :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Bool -> Text -> m a -> m a
collapsibleSectionWithDefault collapsedByDefault header content = divClass "collapsible" $ do
  click <- divClass "collapsible-header" $ do
     fmap (_link_clicked) $ el "strong" $ link header
  collapsed <- fmap collapse <$> toggle collapsedByDefault click
  elDynAttr "div" collapsed content
  where
    collapse b = "style" =: ("display: " <> if b then "none" else "block") <>
      "class" =: "collapsible-content"

-- | Typeahead that displays selected items using pills, handles backspace and clicking to remove items
typeaheadMulti :: forall k t m. (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Ord k)
               => Text -- ^ Placeholder text for input
               -> (Dynamic t Text -> m (Dynamic t (Map k Text))) -- ^ Query to search results function
               -> m (Dynamic t (Set k)) -- ^ Selections
typeaheadMulti ph getter = divClass "typeahead-multi" $ do
  rec let attrs = def & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ "placeholder" =: ph
          getter' q = do
            results <- getter q
            return $ Map.difference <$> results <*> (Map.fromList <$> selections)
      -- TODO: Allow user to click a pill and delete it using backspace
      removeEvents <- elDynAttr "span" (fmap (\x -> if odd x then "class" =: "highlight-last" else mempty) bs) $ do
        l <- holdUniqDyn $ fmap reverse selections
        simpleList l $ \x -> elClass "span" "typeahead-pill" $ do
          dynText $ fmap snd x
          close <- fmap (domEvent Click . fst) $ elAttr' "button" ("type" =: "button") $ icon "times fa-fw"
          return $ tag (current x) close
      let remove = switch $ current $ fmap leftmost removeEvents
      (sel, bs) <- elClass "span" "typeahead-multi-input" $ comboBox' attrs getter' (comboBoxListItem noHighlight (\_ v -> v)) (el "ul")
      selections <- foldDyn ($) [] $ leftmost [ fmap (:) sel
                                              , fmap (\x y -> filter (/= x) y) remove
                                              , fmap (\n y -> if even n && n /= 0 then drop 1 y else y) (updated bs)
                                              ]
  return $ fmap (Set.fromList . map fst) selections
  where
    noHighlight _ x = (:[]) $ Highlight_Off x
    comboBox' cfg getOptions li wrapper = do
      rec (t, inputActions) <- comboBoxInput $ cfg & inputElementConfig_setValue .~ leftmost [fromMaybe never $ _inputElementConfig_setValue cfg, "" <$ selectionE']
          options <- getOptions $ _inputElement_value t
          selectionE <- wrapper $ comboBoxList options li (_inputElement_value t) inputActions
          let selectionE' = attachWithMaybe (\opts k -> fmap (\v -> (k, v)) $ Map.lookup k opts) (current options) selectionE
          let bsAction = () <$ ffilter (== ComboBoxAction_Backspace) inputActions
          bs :: Dynamic t Int <- foldDyn ($) 0 $ leftmost [ (+1) <$ bsAction
                                                          , const 0 <$ updated (_inputElement_value t)
                                                          ]
      return (selectionE', bs)

withFocusSelect :: (MonadWidget t m) => Event t () -> m (TextInput t) -> m (TextInput t)
withFocusSelect focusSelectE mkTextInput  = do
  t <- mkTextInput -- important that this is *before* the following performEvent, which is why we do it ourselves.
  performEvent_ . ffor focusSelectE $ \_ -> do
    let e = _textInput_element t
    IE.select e
    E.focus e
  return t

-- | Create a widget based on a given initial value.  Whenever the given Dynamic
-- is updated to a new, different value, rebuild the widget with the new value.
widgetForDynUniqWithInitial :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Eq a) => a -> Dynamic t a -> (a -> m b) -> m (Dynamic t b)
widgetForDynUniqWithInitial a0 da f = do
  postBuild <- getPostBuild
  deduped <- holdUniqDyn =<< holdDyn a0 (leftmost [updated da, tag (current da) postBuild])
  widgetHold (f a0) $ f <$> updated deduped

elAttrWithoutPropagation' :: forall t m a. (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace)
                          => Text
                          -> Map Text Text
                          -> m a
                          -> m (Element EventResult (DomBuilderSpace m) t, a)
elAttrWithoutPropagation' tagName attrs i = do
  let attrs' = Map.mapKeys (AttributeName Nothing) attrs
  let f = GhcjsEventFilter $ \_ -> return (Reflex.Dom.Core.stopPropagation, return $ Just $ EventResult ())
      cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_initialAttributes .~ attrs'
        & elementConfig_modifyAttributes .~ never
        & elementConfig_eventSpec . ghcjsEventSpec_filters .~ DMap.singleton Click f
  Reflex.Dom.Core.element tagName cfg i

draftWatermark :: DomBuilder t m => m ()
draftWatermark = elAttr "div" attrs $ text "DRAFT"
  where attrs = "style" =: T.intercalate ";"
          [ "position: absolute"
          , "left: 0"
          , "right: 0"
          , "top: 0"
          , "bottom: 0"
          , "transform: rotate(-45deg)"
          , "align-items: center"
          , "display: flex"
          , "justify-content: center"
          , "pointer-events: none"
          , "opacity: 0.1"
          , "font-size: 30vmin"
          ]

data PillAction = PillAction_Add Text
                | PillAction_Remove Text
                | PillAction_RemoveLast
  deriving (Show, Eq)

pillTypeahead
  :: ( DomBuilder DomTimeline m, PostBuild DomTimeline m
     , MonadHold DomTimeline m, MonadFix m )
  => (Text -> Bool)
  -> (Dynamic DomTimeline Text -> m (Dynamic DomTimeline (AppendMap Text v)))
  -> m (Dynamic DomTimeline (Set Text))
pillTypeahead validate get = do
  rec ps <- pills $ leftmost
        [ PillAction_Add <$> sel
        , PillAction_Add <$> fmapMaybe id inputChecked
        , attachWithMaybe (\v -> \case
            ComboBoxAction_Backspace -> if T.null v
              then Just PillAction_RemoveLast
              else Nothing
            _ -> Nothing) (current $ value i) actions
        ]
      let selected = Set.fromList <$> ps
          inputChecked = attachWith (\v _ -> if validate v
            then Just v
            else Nothing) (current $ value i) $ keypress Enter i
          mkInputAttrs = mapKeysToAttributeName . \case
            Left Nothing -> "class" =: Just "error"
            Left (Just _) -> "class" =: Nothing
            Right () -> "class" =: Nothing
          inputAttrs = fmap mkInputAttrs $ leftmost
            [ Left <$> inputChecked
            , Right () <$ updated (value i)
            ]
      (i, actions) <- comboBoxInput $ def
        & inputElementConfig_setValue .~ ("" <$ updated ps)
        & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ inputAttrs
      sel <- comboBoxList xs (comboBoxListItem (\_ x -> [Highlight_Off x]) (\k _ -> k)) (value i) actions
      got <- get (value i)
      let xs = zipDynWith (\vals -> AMap._unAppendMap . AMap.difference vals . AMap.fromSet (\_ -> ())) got selected
  return selected

pills
  :: ( DomBuilder DomTimeline m, PostBuild DomTimeline m
     , MonadHold DomTimeline m, MonadFix m )
  => Event DomTimeline PillAction       -- Take an action on pills
  -> m (Dynamic DomTimeline [Text])     -- Some action
pills e = do
  let update = \p b -> case p of
        PillAction_Add a -> if a `L.elem` b then b else b ++ [a]
        PillAction_Remove a -> L.delete a b
        PillAction_RemoveLast -> reverse $ tail $ reverse b
  rec ps <- foldDyn update [] $ leftmost
        [ e
        , PillAction_Remove <$> (switch . current $ fmap leftmost a)
        ]
      a <- simpleList ps $ \p -> fmap (tag (current p) . domEvent Click . fst) $
        elAttr' "button" ("class" =: "pill") $ do
          dynText p
          elClass "i" "fa fa-blank fa-fw" blank
          elClass "i" "fa fa-close fa-fw" blank
  return ps
