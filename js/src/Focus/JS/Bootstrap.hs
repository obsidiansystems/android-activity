{-# LANGUAGE RecursiveDo, RankNTypes, ScopedTypeVariables #-}
module Focus.JS.Bootstrap where

import Reflex.Dom hiding (button)

import Focus.JS.FontAwesome
import Focus.JS.Widget
import Focus.Schema
import Focus.Time

import Control.Monad
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Time
import Data.Time.LocalTime.TimeZone.Series
import qualified Data.Map as Map
import qualified Data.Text as T

bootstrapCDN :: MonadWidget t m => m ()
bootstrapCDN = elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css") $ return ()

button :: MonadWidget t m => String -> m (Event t ())
button s = buttonClass "btn btn-primary" s

buttonClass :: MonadWidget t m => String -> String -> m (Event t ())
buttonClass k s = button' k $ text s

button' :: MonadWidget t m => String -> m a -> m (Event t ())
button' k w = buttonDynAttr (constDyn $ "class" =: k) w

buttonDynAttr :: MonadWidget t m => Dynamic t (Map String String) -> m a -> m (Event t ())
buttonDynAttr attrs w = liftM (domEvent Click . fst) $ elDynAttr' "button" attrs w

panelContainer :: forall t m a. MonadWidget t m => m a -> m a
panelContainer = divClass "panel panel-primary"

panelHeader :: forall t m a. MonadWidget t m => m a -> m a
panelHeader = divClass "panel-heading"

panelBody :: forall t m a. MonadWidget t m => m a -> m a
panelBody = divClass "panel-body"

panelFooter :: forall t m a. MonadWidget t m => m a -> m a
panelFooter = divClass "panel-footer"

dl :: forall t m a. MonadWidget t m => m a -> m a
dl = elAttr "dl" (Map.singleton "class" "dl-horizontal")

dayInput :: MonadWidget t m => Day -> m (Dynamic t Day)
dayInput d0 = do
  rec day <- foldDyn ($) d0 navigate
      navigate <- do
        (prevButton, nextButton) <- divClass "text-center" $ do
          p <- linkClass "<<" "btn btn-sm pull-left"
          dynText =<< mapDyn (\d -> show (dayToMonth d) <> " " <> show (dayToYear d)) day
          n <- linkClass ">>" "btn btn-sm pull-right"
          return (p, n)
        mc :: Event t (Event t Int) <- dyn =<< mapDyn monthCal day
        dateNav <- liftM (fmap setDayDate . switch) $ hold never mc
        return $ leftmost [fmap (const $ addGregorianMonthsClip (-1)) (_link_clicked prevButton), fmap (const $ addGregorianMonthsClip 1) (_link_clicked nextButton), dateNav]
  return day

monthCal :: forall t m. MonadWidget t m => Day -> m (Event t Int)
monthCal startingDate = do
  let (y, m, d0) = toGregorian startingDate
      som = fromGregorian y m 1
      wd = dayToWeekDay som
      monthLength = dayToMonthLength som
      prefix = map (\x -> (x, Nothing)) $ fst $ break ((==) wd) [Sunday .. Saturday]
      datesAndWeekDays = (<>) prefix $ zip (cycle [wd..]) $ map Just [dayToDate som..monthLength]
      toWeeks x = case x of
                     [] -> []
                     xs -> take 7 xs : toWeeks (drop 7 xs)
      weeks :: [[Maybe Int]] = toWeeks $ map snd datesAndWeekDays
  rec click <- elClass "table" "table-condensed" $ do
        el "thead" $ el "tr" $ do
          forM_ [Sunday .. Saturday] $ \d -> do
            elClass "th" "text-center" $ text $ take 1 $ show d
        el "tbody" $ forM weeks $ \w -> do
          el "tr" $ forM w $ \n -> do
            let attrs = maybe mempty (const $ "style" =: "cursor: pointer;" <> "class" =: "mouseover-active") n
            active <- forDyn sel $ \s -> "class" =: ("btn btn-xs" ++ if s == n && isJust s then " btn-primary" else "")
            (e, _) <- elAttr' "td" attrs $ elDynAttr "a" active $ text $ maybe "" show n
            return $ fmap (const n) $ domEvent Click e
      let selectionMade = leftmost $ concat click
      sel <- holdDyn (Just d0) selectionMade
  return $ fmapMaybe id selectionMade

timeInput :: forall t m. MonadWidget t m => TimeOfDay -> m (Dynamic t TimeOfDay)
timeInput t0 = do
  let hs = Map.map show $ Map.fromList $ zip [(1::Int)..12] [(1::Int)..12]
      ms = Map.fromList $ zip [(0::Int)..59] (map paddingZero [(0::Int)..59])
      twelveHrTime = localTimeToTwelveHourTime t0
      attrs = constDyn $ "class" =: "form-control" <> "style" =: "display: inline-block; width: auto;"
  hour <- liftM value $ dropdown (fst3 twelveHrTime) (constDyn hs) (def & attributes  .~ attrs)
  text ":"
  minute <- liftM value $ dropdown (snd3 twelveHrTime) (constDyn ms) (def & attributes  .~ attrs)
  meridian0 :: Event t Meridian <- liftM (fmap (const $ thd3 twelveHrTime)) getPostBuild
  meridian :: Dynamic t Meridian <- liftM value $ enumDropdown showPretty (def & attributes .~ attrs & setValue .~ meridian0)
  milHour <- combineDyn (\h m -> case m of
                                      AM -> if h == 12 then 0 else h
                                      PM -> if h == 12 then 12 else h + 12) hour meridian
  tod <- liftM (fmapMaybe id . updated) $ combineDyn (\h m -> makeTimeOfDayValid h m 0) milHour minute
  holdDyn t0 tod

utcTimeInputMini :: forall t m. MonadWidget t m => TimeZoneSeries -> UTCTime -> m (Dynamic t UTCTime)
utcTimeInputMini tz t = do
  rec (e', attrs) <- elAttr' "div" ("class" =: "input-group pointer") $ do
        elClass "span" "input-group-addon" $ icon "clock-o"
        _ <- textInput $ def & attributes .~ (constDyn $ "class" =: "form-control" <> "disabled" =: "" <> "style" =: "cursor: pointer; background-color: #fff;")
                        & textInputConfig_setValue .~ fmap (showDateTime' tz) time
                        & textInputConfig_initialValue .~ (showDateTime' tz) t
        isOpen <- holdDyn False $ leftmost [fmap (const False) time, fmap (const True) (domEvent Click e'), fmap (const False) close]
        attrs' :: Dynamic t (Map String String) <- mapDyn (\x -> if x then "class" =: "dropdown-menu" <> "style" =: "width: auto; position: absolute; display: block;" else "class" =: "dropdown-menu") isOpen
        return attrs'
      (time, close) <- elDynAttr "div" attrs $ do
        d <- dayInput (localDay $ utcToLocalTime' tz t)
        lt <- divClass "form-inline text-center" $ do
          t' <- timeInput (localTimeOfDay $ utcToLocalTime' tz t)
          lt <- combineDyn (\day t'' -> LocalTime day t'') d t'
          return lt
        elAttr "div" ("class" =: "btn-group" <> "style" =: "padding-top: 5px; width: 90%; left: 5%;") $ do
          (a, _) <- elAttr' "a" ("class" =: "btn btn-primary btn-sm width50") (icon "check")
          (x, _) <- elAttr' "a" ("class" =: "btn btn-default btn-sm width50") (icon "times")
          return $ (fmap (localTimeToUTC' tz) $ tag (current lt) $ domEvent Click a, domEvent Click x)
  holdDyn t time

--TODO better way to sort lists
mapSort :: (Ord k, Ord s) => (v -> s) -> SortOrder -> Map k v -> Map (Int,k) v
mapSort f order m =
  let sorted = (sortBy $ comparing (f . snd)) $ Map.toList m
      toMap = Map.fromList . map (\(x,(y,z)) -> ((x,y),z)) . zip [1..]
  in case order of
          Ascending -> toMap sorted
          Descending -> toMap $ reverse sorted

data SortOrder = Ascending
               | Descending
               deriving (Show, Read, Eq, Ord, Enum, Bounded)

sortList :: forall t m k v a. (MonadWidget t m, Ord k) => Dynamic t (Map k v) -> Event t (Maybe k) -> (v -> String) -> (Maybe (Int, k) -> Dynamic t v -> Dynamic t Bool -> m (Event t a)) -> m (Dynamic t (Maybe k))
sortList items setK toString mkChild = do
  sortOrder :: Dynamic t SortOrder <- divClass "pull-right" $ alphaSortSelector
  query <- searchBox "search"
  sorted <- combineDyn (\s vs -> Map.fromList $ map (\(k, v) -> (Just k, v)) $ Map.toList $ mapSort toString s vs) sortOrder items
  result <- combineDyn (\q xs -> Map.filter (substringFilter q . toString) xs) query sorted
  let setSel = attachWithMaybe (\xs k -> case k of
                 Nothing -> Just Nothing
                 Just k' -> case filter (\(_, mk) -> mk == k')$ catMaybes $ Map.keys xs of
                                 [] -> Nothing
                                 key:_ -> Just $ Just key) (current sorted) setK
  rec sel :: Event t (Maybe (Int, k)) <- el "div" $ divClass "list-group" $ selectViewListWithKey_ curSel result mkChild
      curSel <- holdDyn Nothing $ leftmost [setSel, sel]
  mapDyn (fmap snd) curSel

sortListWithDeselectButton :: (Ord k, MonadWidget t m) => String -> Dynamic t (Map k v) -> (v -> String) -> (Maybe (Int, k) -> Dynamic t v -> Dynamic t Bool -> m (Event t a )) -> m (Dynamic t (Maybe k))
sortListWithDeselectButton buttonLabel items toString mkChild = do
  b <- button buttonLabel
  let deselect = fmap (const Nothing) b
  sortList items deselect toString mkChild

substringFilter :: Maybe String -> String -> Bool
substringFilter q s = case q of
                           Nothing -> True
                           Just "" -> True
                           Just q' -> T.count (T.toLower $ T.pack q') (T.toLower $ T.pack s) > 0

alphaSortSelector :: MonadWidget t m => m (Dynamic t SortOrder)
alphaSortSelector = do
  divClass "btn-group" $ do
    rec asc <- buttonDynAttr attrsA $ icon "sort-alpha-asc"
        desc <- buttonDynAttr attrsD $ icon "sort-alpha-desc"
        order <- holdDyn Ascending $ leftmost [fmap (const Ascending) asc, fmap (const Descending) desc]
        let active = "class" =: "btn btn-default active"
            inactive = "class" =: "btn btn-default"
        attrsA <- forDyn order $ \x -> if x == Ascending then active else inactive
        attrsD <- forDyn order $ \x -> if x == Descending then active else inactive
    return order

searchBox' :: MonadWidget t m => String -> String -> m (Dynamic t (Maybe String))
searchBox' i p = do
  divClass "input-group" $ do
    elClass "span" "input-group-addon" $ icon i
    q <- liftM value (textInputWithPlaceholder p)
    forDyn q $ \v -> let x = T.unpack $ T.strip $ T.pack v
                     in if x == "" then Nothing else Just x

searchBox :: MonadWidget t m => String -> m (Dynamic t (Maybe String))
searchBox i = searchBox' i "Search..."

inputWithPlaceholder' :: forall t m. MonadWidget t m => String -> String -> String -> Event t String -> m (TextInput t)
inputWithPlaceholder' inputType initial p eSetVal = textInput $ def & textInputConfig_inputType .~ inputType
                                                                    & textInputConfig_initialValue .~  initial
                                                                    & setValue .~ eSetVal
                                                                    & attributes .~ constDyn (Map.fromList [("class", "form-control"), ("placeholder", p)])

inputWithPlaceholder :: forall t m. MonadWidget t m => String -> String -> m (TextInput t)
inputWithPlaceholder inputType p = inputWithPlaceholder' inputType "" p never

textInputWithPlaceholder :: forall t m. MonadWidget t m => String -> m (TextInput t)
textInputWithPlaceholder p = inputWithPlaceholder "text" p

passwordInputWithPlaceholder :: forall t m. MonadWidget t m => String -> m (TextInput t)
passwordInputWithPlaceholder p = inputWithPlaceholder "password" p

emailInputWithPlaceholder :: forall t m. MonadWidget t m => String -> m (TextInput t)
emailInputWithPlaceholder p = inputWithPlaceholder "email" p

labelledInput :: forall t m a. MonadWidget t m => String -> m a -> m a
labelledInput name content = do
  divClass "form-group" $ do
    elAttr "label" (Map.fromList [("for", ""), ("class", "col-sm-2 control-label")]) $ do
      text $ name
    divClass "col-sm-10" $ return =<< content

buttonWithIcon :: forall t m. MonadWidget t m => String -> String -> String -> m (Event t ())
buttonWithIcon i s btnClass = button' btnClass $ do
  icon i
  text $ " " <> s

stamp :: MonadWidget t m => String -> String -> m ()
stamp k v = elAttr "span" ("class" =: ("stamp " <> k) <> stampDefaultStyle) $ text v

stamp' :: MonadWidget t m => String -> String -> m ()
stamp' k v = elAttr "span" ("class" =: ("stamp " <> k)) $ text v

stampDefaultStyle = "style" =: "color: white; text-transform: uppercase; font-weight: bold; padding-left: 0.25em; padding-right: 0.25em; font-size: small; border-radius: 0.25em; box-shadow: 1px 1px 1px black; text-shadow: 1px 1px 1px black;"

tristateButton :: MonadWidget t m => String -> String -> Dynamic t (Maybe Bool) -> m (Event t ())
tristateButton k l b = do
  attrs <- mapDyn buttonAttrs b
  buttonDynAttr attrs (dyn =<< mapDyn buttonContents b)
  where
    buttonAttrs x = case x of
                         Nothing -> "class" =: ("btn btn-primary " <> k)
                         Just True -> "class" =: ("btn btn-success disabled " <> k)
                         Just False -> "class" =: ("btn btn-default disabled " <> k)
    buttonContents x = case x of
                            Just True -> do
                              text $ l <> " "
                              icon "check"
                            _ -> text l

maybeBadge :: forall t m a. (MonadWidget t m, Num a, Show a) => Dynamic t (Maybe a) -> m (Event t ())
maybeBadge mInt = dyn =<< mapDyn (maybe blank $ (\n -> elAttr "span" (Map.singleton "class" "badge") $ text $ show n)) mInt

checkboxWithLabel :: forall t m. MonadWidget t m => String -> m (Checkbox t)
checkboxWithLabel l = checkboxWithLabelAttrs l Map.empty

checkboxWithLabelAttrs :: forall t m. MonadWidget t m => String -> Map String String -> m (Checkbox t)
checkboxWithLabelAttrs l attrs = elAttr "label" attrs $ do
  c <- checkbox False def
  text $ " " <> l
  return c

progressBar :: MonadWidget t m => String -> Dynamic t Double -> m ()
progressBar k w = divClass "progress" $ do
  attrs <- forDyn w (\w' -> "class" =: ("progress-bar progress-bar-" <> k) <> "style" =: ("width: " <> show w' <> "%"))
  elDynAttr "div" attrs $ return ()
