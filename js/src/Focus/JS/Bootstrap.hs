{-# LANGUAGE RecursiveDo, RankNTypes, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TemplateHaskell, QuasiQuotes, LambdaCase, OverloadedStrings, DeriveFunctor #-}
module Focus.JS.Bootstrap where

import Reflex.Dom hiding (button)

import Focus.JS.FontAwesome
import Focus.JS.Time
import Focus.JS.Widget
import Focus.Schema
import Focus.Time
import Focus.Misc
import Focus.Account
import Focus.Sign
import Focus.JS.Prerender

import Safe
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
-- import Control.Monad.Ref
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import Data.Time
import Data.Time.LocalTime.TimeZone.Series
import qualified Data.Map as Map
import qualified Data.Text as T
import Text.RawString.QQ

bootstrapCDN :: DomBuilder t m => m ()
bootstrapCDN = elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css") $ return ()

button :: (DomBuilder t m, PostBuild t m) => Text -> m (Event t ())
button s = buttonClass "btn btn-primary" s

buttonClass :: (DomBuilder t m, PostBuild t m) => Text -> Text -> m (Event t ())
buttonClass k s = button' k $ text s

buttonActiveClass :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Bool -> Text -> m (Event t ())
buttonActiveClass k actD s = buttonActive' k actD (text s)

button' :: (DomBuilder t m, PostBuild t m) => Text -> m a -> m (Event t ())
button' k w = buttonAttr ("class" =: k <> "type" =: "button") w

buttonActive' :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Bool -> m a -> m (Event t ())
buttonActive' k actD w = do
  let attrs = ffor actD $ \active -> "class" =: k <> "type" =: "button" <> if active then mempty else "disabled" =: "1"
  buttonDynAttr attrs w

buttonAttr :: (DomBuilder t m, PostBuild t m) => Map Text Text -> m a -> m (Event t ())
buttonAttr attrs w = liftM (domEvent Click . fst) $ elAttr' "button" attrs w

buttonDynAttr :: (DomBuilder t m, PostBuild t m) => Dynamic t (Map Text Text) -> m a -> m (Event t ())
buttonDynAttr attrs w = liftM (domEvent Click . fst) $ elDynAttr' "button" attrs w

panelContainer :: forall t m a. DomBuilder t m => m a -> m a
panelContainer = divClass "panel panel-primary"

panelHeader :: forall t m a. DomBuilder t m => m a -> m a
panelHeader = divClass "panel-heading"

panelBody :: forall t m a. DomBuilder t m => m a -> m a
panelBody = divClass "panel-body"

panelFooter :: forall t m a. DomBuilder t m => m a -> m a
panelFooter = divClass "panel-footer"


dl :: forall t m a. DomBuilder t m => m a -> m a
dl = elAttr "dl" (Map.singleton "class" "dl-horizontal")

data NumberInput er d t a = NumberInput
  { _numberInput_value :: Dynamic t (Maybe a)
  , _numberInput_textInput :: InputElement er d t
  }

instance HasValue (NumberInput er d t a) where
  type Value (NumberInput er d t a) = Dynamic t (Maybe a)
  value = _numberInput_value

numberInput :: (Num a, DomBuilder t m) => Text -> m (NumberInput EventResult (DomBuilderSpace m) t a)
numberInput initial = do
  ti <- inputElement $ def
    & inputElementConfig_initialValue .~ initial
    & initialAttributes .~ ("class" =: "form-control" <> "type" =: "number")
  let v = fmap (fmap fromInteger . readMay . T.unpack) (value ti)
  return (NumberInput v ti)

dayInput :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Day -> m (Dynamic t Day)
dayInput d0 = do
  let (year0, month0, _ {- dayOfMonth0 -} ) = toGregorian d0
  rec visibleMonth <- foldDyn ($) (year0, intToMonth month0) navigate'
      (navigate', dayClicked') <- do
        (prevButton, nextButton) <- divClass "text-center" $ do
          p <- linkClass "<<" "btn btn-sm pull-left"
          elAttr "span" ("style" =: "position:relative; top: 10px") $ dynText $ fmap (\(y, m) -> T.pack (show m) <> " " <> T.pack (show y)) visibleMonth
          n <- linkClass ">>" "btn btn-sm pull-right"
          return (p, n)
        mc :: Event t (Event t Int) <- dyn $ ffor visibleMonth $ \(y, m) -> do
          let mDayOfMonth = ffor day $ \d ->
                let (y', m', dom') = toGregorian d
                in if (y', intToMonth m') == (y, m)
                   then Just dom'
                   else Nothing
          monthCal y m mDayOfMonth
        dayClicked <- liftM switch $ hold never mc
        let navigate = leftmost [ addMonths (-1) <$ _link_clicked prevButton
                                , addMonths 1 <$ _link_clicked nextButton
                                ]
        return (navigate, dayClicked)
      day <- holdDyn d0 $ attachPromptlyDynWith (\(y, m) d -> fromGregorian y (monthToInt m) d) visibleMonth dayClicked'
  return day

-- Allows selecting a month/year (preserves the day of the month)
monthInput :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Day -> m (Dynamic t Day)
monthInput d0 = do
  let (year0, month0, day0) = toGregorian d0
  rec visibleMonth <- foldDyn ($) (year0, intToMonth month0) navigate'
      navigate' <- do
        (prevButton, nextButton) <- divClass "text-center" $ do
          p <- linkClass "<<" "btn btn-sm pull-left"
          elAttr "span" ("style" =: "position:relative; top: 10px") $ dynText $ fmap (\(y, m) -> T.pack (show m) <> " " <> T.pack (show y)) visibleMonth
          n <- linkClass ">>" "btn btn-sm pull-right"
          return (p, n)
        return $ leftmost [ addMonths (-1) <$ _link_clicked prevButton
                          , addMonths 1 <$ _link_clicked nextButton
                          ]
  return $ ffor visibleMonth $ \(y, m) -> fromGregorian y (monthToInt m) day0

monthCal :: forall t m. (DomBuilder t m, PostBuild t m, MonadFix m) => Integer -> Month -> Dynamic t (Maybe Int) -> m (Event t Int)
monthCal y m sel = do
  let som = fromGregorian y (monthToInt m) 1
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
            elClass "th" "text-center" $ text $ T.pack $ take 1 $ show d
        el "tbody" $ forM weeks $ \w -> do
          el "tr" $ forM w $ \n -> do
            let attrs = maybe mempty (const $ "style" =: "cursor: pointer;" <> "class" =: "mouseover-active") n
                active = ffor sel $ \s -> "class" =: ("btn btn-xs" <> if s == n && isJust s then " btn-primary" else "")
            (e, _) <- elAttr' "td" attrs $ elDynAttr "a" active $ text $ T.pack $ maybe "" show n
            return $ fmap (const n) $ domEvent Click e
  let selectionMade = leftmost $ concat click
  return $ fmapMaybe id selectionMade

timeInput :: forall t m. (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => TimeOfDay -> m (Dynamic t TimeOfDay)
timeInput t0 = do
  let hs = Map.map (T.pack . show) $ Map.fromList $ zip [(1::Int)..12] [(1::Int)..12]
      ms = Map.fromList $ zip [(0::Int)..59] (map paddingZero [(0::Int)..59])
      twelveHrTime = localTimeToTwelveHourTime t0
      attrs = constDyn $ "class" =: "form-control" <> "style" =: "display: inline-block; width: auto;"
  hour <- liftM value $ dropdown (fst3 twelveHrTime) (constDyn hs) (def & attributes .~ attrs)
  text ":"
  minute <- liftM value $ dropdown (snd3 twelveHrTime) (constDyn ms) (def & attributes .~ attrs)
  let meridian0 = thd3 twelveHrTime
  meridian :: Dynamic t Meridian <- liftM value $ enumDropdown' meridian0 showPretty (def & attributes .~ attrs)
  let milHour = zipDynWith (\h m -> case m of
        AM -> if h == 12 then 0 else h
        PM -> if h == 12 then 12 else h + 12) hour meridian
      tod = fmapMaybe id . updated $ zipDynWith (\h m -> makeTimeOfDayValid h m 0) milHour minute
  holdDyn t0 tod

mainlandUSTimeZoneMap :: (MonadIO m, HasJS x m) => m (Map Text TimeZoneSeries)
mainlandUSTimeZoneMap = do
  kvs <- forM ["Eastern", "Central", "Mountain", "Pacific"] $ \n ->
            do s <- getTimeZoneSeries ("US/" <> n)
               return (n,s)
  return (Map.fromList [(n,s) | (n,Just s) <- kvs])

mainlandUSTimeInput :: (HasJS x m, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Map Text TimeZoneSeries -> UTCTime -> m (Dynamic t UTCTime)
mainlandUSTimeInput tzMap t0 =
  utcTimeInputMini (tzMap Map.! "Eastern") (mainlandUSTimeZone tzMap def) t0

mainlandUSTimeZone :: (HasJS x m, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Map Text TimeZoneSeries -> DropdownConfig t Text -> m (Dynamic t TimeZoneSeries)
mainlandUSTimeZone tzMap _ {- cfg -} = do
  let labelMap, valueMap :: Map Int Text
      labelMap = 0 =: "PT"      <> 1 =: "MT"       <> 2 =: "CT"      <> 3 =: "ET"
      valueMap = 0 =: "Pacific" <> 1 =: "Mountain" <> 2 =: "Central" <> 3 =: "Eastern"
  selection <- fmap (fmap (valueMap Map.!)) $ toggleButtonStrip "btn-xs" 3 labelMap
  return $ fmap (tzMap Map.!) selection

utcTimeInputMini :: (HasJS x m, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
                 => TimeZoneSeries
                 -> m (Dynamic t TimeZoneSeries)
                 -> UTCTime
                 -> m (Dynamic t UTCTime)
utcTimeInputMini tz0 tzWidget t = do
  rec let timeShown = zipDynWith (\tz t' -> showDateTime' tz t') tzD timeD
      (e', attrs) <- elAttr' "div" ("class" =: "input-group pointer") $ do
        elClass "span" "input-group-addon" $ icon "clock-o"
        _ <- inputElement $ def
          & initialAttributes .~ ("class" =: "form-control" <> "readonly" =: "" <> "style" =: "cursor: pointer; background-color: #fff;")
          & inputElementConfig_setValue .~ updated timeShown
          & inputElementConfig_initialValue .~ showDateTime' tz0 t
        isOpen <- holdDyn False $ leftmost [fmap (const True) (domEvent Click e'), fmap (const False) close]
        return $ ffor isOpen $ \x -> if x then "class" =: "dropdown-menu" <> "style" =: "width: auto; position: absolute; display: block;"
                                          else "class" =: "dropdown-menu"
      (timeD, tzD, close) <- elDynAttr "div" attrs $ do
        close' <- liftM (domEvent Click . fst) $ elAttr' "div" ("class" =: "modal-background" <> "style" =: "position:fixed; background-color: rgba(0,0,0,0)") $ return ()
        elAttr "div" ("style" =: "z-index:20; position:relative") $ do
          rec ltD <- divClass "form-inline text-center" $ do
                t' <- timeInput (localTimeOfDay $ utcToLocalTime' tz0 t)
                return $ zipDynWith (\day t'' -> LocalTime day t'') d t'
              tzD <- elAttr "div" ("style" =: "padding-top: 5px") $ tzWidget
              d <- dayInput (localDay $ utcToLocalTime' tz0 t)
              let timeD' = zipDynWith (\tz lt -> localTimeToUTC' tz lt) tzD ltD
          return (timeD', tzD, close')
  return timeD

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

sortList :: forall t m k v a. (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Ord k) => Dynamic t (Map k v) -> Event t (Maybe k) -> (v -> Text) -> (Maybe (Int, k) -> Dynamic t v -> Dynamic t Bool -> m (Event t a)) -> m (Dynamic t (Maybe k))
sortList items setK toString mkChild = do
  sortOrder :: Dynamic t SortOrder <- divClass "pull-right" $ alphaSortSelector
  query <- searchBox "search"
  let sorted = zipDynWith (\s vs -> Map.fromList $ map (\(k, v) -> (Just k, v)) $ Map.toList $ mapSort toString s vs) sortOrder items
      result = zipDynWith (\q xs -> Map.filter (substringFilter q . toString) xs) query sorted
      setSel = attachWithMaybe (\xs k -> case k of
                 Nothing -> Just Nothing
                 Just k' -> case filter (\(_, mk) -> mk == k')$ catMaybes $ Map.keys xs of
                                 [] -> Nothing
                                 key:_ -> Just $ Just key) (current sorted) setK
  rec sel :: Event t (Maybe (Int, k)) <- el "div" $ divClass "list-group" $ selectViewListWithKey_ curSel result mkChild
      curSel <- holdDyn Nothing $ leftmost [setSel, sel]
  return $ fmap snd <$> curSel

sortListWithDeselectButton :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Ord k) => Text -> Dynamic t (Map k v) -> (v -> Text) -> (Maybe (Int, k) -> Dynamic t v -> Dynamic t Bool -> m (Event t a )) -> m (Dynamic t (Maybe k))
sortListWithDeselectButton buttonLabel items toString mkChild = do
  b <- button buttonLabel
  let deselect = fmap (const Nothing) b
  sortList items deselect toString mkChild

substringFilter :: Maybe Text -> Text -> Bool
substringFilter q s = case q of
                           Nothing -> True
                           Just "" -> True
                           Just q' -> T.count (T.toLower q') (T.toLower s) > 0

alphaSortSelector :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Dynamic t SortOrder)
alphaSortSelector = do
  divClass "btn-group" $ do
    rec asc <- buttonDynAttr attrsA $ icon "sort-alpha-asc"
        desc <- buttonDynAttr attrsD $ icon "sort-alpha-desc"
        order <- holdDyn Ascending $ leftmost [fmap (const Ascending) asc, fmap (const Descending) desc]
        let active = "class" =: "btn btn-default active"
            inactive = "class" =: "btn btn-default"
            attrsA = ffor order $ \x -> if x == Ascending then active else inactive
            attrsD = ffor order $ \x -> if x == Descending then active else inactive
    return order

searchBox' :: DomBuilder t m => Text -> Text -> m (Dynamic t (Maybe Text))
searchBox' i p = do
  divClass "input-group" $ do
    elClass "span" "input-group-addon" $ icon i
    q <- value <$> textInputWithPlaceholder p
    return $ ffor q $ \v -> let x = T.strip v in if x == "" then Nothing else Just x

searchBox :: DomBuilder t m => Text -> m (Dynamic t (Maybe Text))
searchBox i = searchBox' i "Search..."

readonlyInput :: forall t m. DomBuilder t m => Text -> Text -> Text -> Event t Text -> m (InputElement EventResult (DomBuilderSpace m) t)
readonlyInput inputType initial _ eSetVal = inputElement $ def
  & inputElementConfig_initialValue .~ initial
  & inputElementConfig_setValue .~ eSetVal
  & initialAttributes .~ ("class" =: "form-control" <> "readonly" =: "readonly" <> "type" =: inputType)

inputWithPlaceholder' :: forall t m. DomBuilder t m => Text -> Text -> Text -> Event t Text -> m (InputElement EventResult (DomBuilderSpace m) t)
inputWithPlaceholder' inputType initial p eSetVal = inputElement $ def
  & inputElementConfig_initialValue .~  initial
  & inputElementConfig_setValue .~ eSetVal
  & initialAttributes .~ ("class" =: "form-control" <> "placeholder" =: p <> "type" =: inputType)

inputWithPlaceholder :: forall t m. DomBuilder t m => Text -> Text -> m (InputElement EventResult (DomBuilderSpace m) t)
inputWithPlaceholder inputType p = inputWithPlaceholder' inputType "" p never

textInputWithPlaceholder :: forall t m. DomBuilder t m => Text -> m (InputElement EventResult (DomBuilderSpace m) t)
textInputWithPlaceholder p = inputWithPlaceholder "text" p

passwordInputWithPlaceholder :: forall t m. DomBuilder t m => Text -> m (InputElement EventResult (DomBuilderSpace m) t)
passwordInputWithPlaceholder p = inputWithPlaceholder "password" p

emailInputWithPlaceholder :: forall t m. DomBuilder t m => Text -> m (InputElement EventResult (DomBuilderSpace m) t)
emailInputWithPlaceholder p = inputWithPlaceholder "email" p

labelledInput :: forall t m a. DomBuilder t m => Text -> m a -> m a
labelledInput name content = do
  divClass "form-group" $ do
    elAttr "label" (Map.fromList [("for", ""), ("class", "col-sm-2 control-label")]) $ do
      text $ name
    divClass "col-sm-10" $ return =<< content

buttonWithIcon :: forall t m. (DomBuilder t m, PostBuild t m) => Text -> Text -> Text -> m (Event t ())
buttonWithIcon i s btnClass = button' btnClass $ do
  icon i
  text $ " " <> s

stamp :: DomBuilder t m => Text -> Text -> m ()
stamp k v = elAttr "span" ("class" =: ("stamp " <> k) <> stampDefaultStyle) $ text v

stamp' :: DomBuilder t m => Text -> Text -> m ()
stamp' k v = stampWidget k $ text v

stampWidget :: DomBuilder t m => Text -> m a -> m a
stampWidget k w = elAttr "span" ("class" =: ("stamp " <> k)) w

stampDefaultStyle :: Map Text Text
stampDefaultStyle = "style" =: "color: white; text-transform: uppercase; font-weight: bold; padding-left: 0.25em; padding-right: 0.25em; font-size: small; border-radius: 0.25em; box-shadow: 1px 1px 1px black; text-shadow: 1px 1px 1px black;"

tristateButton :: (DomBuilder t m, PostBuild t m) => Text -> Text -> Dynamic t (Maybe Bool) -> m (Event t ())
tristateButton k l b = do
  let attrs = fmap buttonAttrs b
  buttonDynAttr attrs (dyn $ fmap buttonContents b)
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

maybeBadge :: forall t m a. (DomBuilder t m, PostBuild t m, Show a) => Dynamic t (Maybe a) -> m (Event t ())
maybeBadge mInt = dyn $ ffor mInt $ maybe blank $ \n -> elAttr "span" (Map.singleton "class" "badge") $ text $ T.pack $ show n

checkboxWithLabel :: forall t m. (DomBuilder t m, PostBuild t m) => Text -> m (Checkbox t)
checkboxWithLabel l = checkboxWithLabelAttrs l Map.empty

checkboxWithLabelAttrs :: forall t m. ((DomBuilder t m, PostBuild t m)) => Text -> Map Text Text -> m (Checkbox t)
checkboxWithLabelAttrs l attrs = elAttr "label" attrs $ do
  c <- checkbox False def
  text $ " " <> l
  return c

progressBar :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Double -> m ()
progressBar k w = divClass "progress" $ do
  let attrs = ffor w $ \w' -> "class" =: ("progress-bar progress-bar-" <> k) <> "style" =: ("width: " <> T.pack (show w') <> "%")
  elDynAttr "div" attrs $ return ()

maybeDisplay :: forall t m a b. (DomBuilder t m, PostBuild t m) => a -> (Dynamic t a -> m (Dynamic t (Maybe b))) -> (a -> b -> m ()) -> m ()
maybeDisplay someId watcher child = do
  a <- watcher (constDyn someId)
  _ <- dyn $ fmap (maybe (text "Loading...") $ child someId) a
  return ()

jumbotron :: forall t m a. DomBuilder t m => Text -> Text -> m a -> m a
jumbotron t subtitle child = divClass "jumbotron" $ do
  elAttr "h1" (Map.singleton "class" "text-center") $ do
    text t
  elAttr "h1" (Map.singleton "class" "text-center") $ do
    el "small" $ text subtitle
  child

withLoginWorkflow'
  :: forall a t m loginInfo newUser recoverResult. (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Monoid a)
  => Bool -- ^ Whether to display sign-up form first
  -> (forall x. m x -> m x)
  -- ^ wrapper widget
  -> Maybe loginInfo
  -- ^ initial login information
  -> m (Event t newUser, Event t ())
  -- ^ New Account (New User, return to signin)
  -> m (Event t recoverResult, Event t ())
  -- ^ Recover (Password Reset Requested, return to signin)
  -> m (Event t loginInfo, Event t ())
  -- ^ Login (Successful login request, return to signup, password reset)
  -> (loginInfo -> m (Event t (), Dynamic t a))
  -- ^ Post-login, returns a logout event
  -> Workflow t m (Event t (Maybe loginInfo), Dynamic t a)
withLoginWorkflow' signUp wrapper li0 newAccountForm' recoveryForm' loginForm' f =
  let loginWorkflow' = Workflow . wrapper $ do
        let newAccountWorkflow = Workflow $ do
              (_ {- eSignupClick -}, eSigninClick) <- newAccountForm'
              return (never, fmap (const loginWorkflow) eSigninClick)
            recoverAccountWorkflow = Workflow $ do
              (_ {- eReset -}, eSigninClick) <- recoveryForm'
              return (never, fmap (const loginWorkflow) eSigninClick)
            loginWorkflow = Workflow $ do
              (eLoginSuccess, eNewAccountClick) <- loginForm'
              recoverLink <- elAttr "p" (Map.singleton "class" "text-center") $ do
                text "Forgot password? "
                link "Recover account"
              let eNewAccount = fmap (const newAccountWorkflow) eNewAccountClick
                  eRecoverAccount = fmap (const recoverAccountWorkflow) (_link_clicked recoverLink)
              let eChange = leftmost [eNewAccount, eRecoverAccount]
              return (eLoginSuccess, eChange)
        eLoginInfo <- liftM switch $ hold never =<< workflowView (if signUp then newAccountWorkflow else loginWorkflow)
        return ((fmap Just eLoginInfo, constDyn mempty), fmap f' eLoginInfo)
      f' x = Workflow $ do
        (eLogout, a) <- f x
        return ((fmap (const Nothing) eLogout, a), fmap (const loginWorkflow') eLogout)
  in maybe loginWorkflow' f' li0

recoveryForm
  :: forall t m recoverResult. (DomBuilder t m, PostBuild t m, MonadHold t m)
  => (Event t Email -> m (Event t recoverResult))
  -- ^ Recover password request
  -> m (Event t recoverResult, Event t ())
recoveryForm requestPasswordResetEmail = elAttr "form" (Map.singleton "class" "form-signin") $ do
  signinLink <- elAttr "h3" (Map.singleton "class" "form-signin-heading") $ do
    text "Recover or "
    linkClass "sign in" "pointer"
  emailBox <- emailInputWithPlaceholder "Email address"
  submitButton <- linkClass "Recover" "btn btn-lg btn-primary btn-block"
  eReset <- requestPasswordResetEmail $ tag (current $ value emailBox) (_link_clicked submitButton)
  el "div" $ elAttr "p" (Map.singleton "class" "text-center") $ dynText =<< holdDyn "" (fmap (const "An email with account recovery instructions has been sent.") eReset)
  return (eReset, (_link_clicked signinLink))

loginForm
  :: forall t m err loginInfo. (DomBuilder t m, MonadFix m)
  => (Event t (Email, Text) -> m (Event t (Either err loginInfo)))
  -- ^ Login request
  -> (Event t err -> m (Event t (Email, Text), Event t ()))
  -- ^ The widget that actually renders the login form and returns a login attempt event or an event to go to the signup page
  -> m (Event t loginInfo, Event t ())
loginForm login formWidget = do
  rec (submit, signup) <- formWidget eLoginFailure
      eLoginResult <- login submit
      -- Maybe add a splitEvent function to Reflex of type Event t a -> (a -> Either b c) -> (Event t b, Event t c)
      let eLoginSuccess = fmapMaybe (\case
                                        Left _ -> Nothing
                                        Right x -> Just x) eLoginResult
          eLoginFailure = fmapMaybe (\case
                                        Left x -> Just x
                                        Right _ -> Nothing) eLoginResult
  return (eLoginSuccess, signup)

-- | A simple default login form for applications without a custom login page design.
defaultLoginForm
  :: forall t m err loginInfo. (DomBuilder t m, MonadFix m, MonadHold t m)
  => (Event t (Email, Text) -> m (Event t (Either err loginInfo)))
  -> m (Event t loginInfo, Event t ())
defaultLoginForm login = loginForm login defaultLoginWidget

defaultLoginWidget
  :: forall t m err. (DomBuilder t m, MonadHold t m)
  => Event t err
  -> m (Event t (Email, Text), Event t ())
defaultLoginWidget errE = elAttr "form" ("class" =: "form-signin") $ do
  signupLink <- elAttr "h3" ("class" =: "form-signin-heading") $ do
    text "Sign in up or "
    link "sign up"
  emailBox <- emailInputWithPlaceholder "Email address"
  passwordBox <- passwordInputWithPlaceholder "Password"
  submitButton <- link "Sign in"
  let credentialsD = do
        email <- value emailBox
        password <- value passwordBox
        return (email, password)
      eEmailEnter = keypress Enter emailBox
      ePasswordEnter = keypress Enter passwordBox
      submitE = tagPromptlyDyn credentialsD $ leftmost [eEmailEnter, ePasswordEnter, _link_clicked submitButton]
      showWarningE = (icon "warning" >> divClass "alert alert-warning" (text "Login unsuccessful.")) <$ errE
  _ <- widgetHold blank showWarningE
  return (submitE, _link_clicked signupLink)

loginFormWithReset
  :: forall t m err loginInfo. (DomBuilder t m, MonadFix m)
  => (Event t (Email, Text) -> m (Event t (Either err loginInfo)))
  -- ^ Login request
  -> (Event t err -> m (Event t (Email, Text), Event t ()))
  -- ^ How to display an error message
  -> m (Event t loginInfo, Event t (), Event t ())
loginFormWithReset login formWidget = do
  (eLoginSuccess, signup) <- loginForm login formWidget
  forgotPasswordLink <- elAttr "h3" (Map.singleton "class" "form-reset-password-heading") $ linkClass "Forgot password?" "pointer"
  return (eLoginSuccess, signup, _link_clicked forgotPasswordLink)

newAccountForm
  :: (DomBuilder t m, PostBuild t m, MonadHold t m)
  => m (Behavior t newUser)
  -- ^ New Account form
  -> (Event t newUser -> m (Event t newUserId))
  -- ^ New Account Request
  -> m (Event t newUserId, Event t ())
newAccountForm newUserForm newUserReq = elAttr "form" (Map.singleton "class" "form-signin") $ do
  signinLink <- elAttr "h3" (Map.singleton "class" "form-signin-heading") $ do
    text "Sign up or "
    linkClass "sign in" "pointer"
  newUser <- newUserForm
  submitButton <- linkClass "Sign up" "btn btn-lg btn-primary btn-block"
  el "div" $ elAttr "p" (Map.singleton "class" "text-center") $ dynText =<< holdDyn "" (fmap (const "An email with account activation instructions has been sent.") (_link_clicked submitButton))
  eNewUser <- newUserReq $ tag newUser (_link_clicked submitButton)
  return (eNewUser, (_link_clicked signinLink))

passwordResetWorkflow
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Signed PasswordResetToken
  -> (Maybe loginInfo -> Workflow t m (Event t (Maybe loginInfo)))
  -> (Event t (Signed PasswordResetToken, Text) -> m (Event t loginInfo))
  -> m (Event t (Maybe loginInfo))
passwordResetWorkflow token withLoginWorkflow reset = do
  liftM switch $ hold never <=< workflowView $ Workflow $ do
    eLoginResult <- liftM (fmap Just) $ passwordReset token reset
    let eLoggedIn = fmap (\x -> withLoginWorkflow x) eLoginResult
    return (eLoginResult, eLoggedIn)

passwordReset
  :: (DomBuilder t m, PostBuild t m)
  => Signed PasswordResetToken
  -> (Event t (Signed PasswordResetToken, Text) -> m (Event t loginInfo))
  -> m (Event t loginInfo)
passwordReset token reset = elAttr "form" (Map.singleton "class" "form-signin") $ do
  elAttr "h3" (Map.singleton "class" "form-signin-heading") $ text "Set Password"
  pw <- passwordInputWithPlaceholder "Password"
  confirm <- passwordInputWithPlaceholder "Confirm Password"
  let dPasswordConfirmed = zipDynWith (==) (value pw) (value confirm)
      dAttrs = fmap (\c -> Map.singleton "class" $ "btn btn-lg btn-primary btn-block" <> if c then "" else " disabled") dPasswordConfirmed
  (submitButton, _) <- elDynAttr' "a" dAttrs $ text "Set Password"
  let enterPressed = gate (current dPasswordConfirmed) $ leftmost [keypress Enter pw, keypress Enter confirm]
      submit = leftmost [domEvent Click submitButton, enterPressed]
  loginInfo <- reset $ fmap (\p -> (token, p)) $ tag (current (value pw)) submit
  --performEvent_ $ fmap (const $ liftIO $ windowHistoryPushState "/") loginInfo
  return loginInfo

styleTagSignin :: DomBuilder t m => m ()
styleTagSignin = el "style" $ text [r|
  .form-signin {
    max-width: 330px;
    padding: 15px;
    margin: 0 auto;
  }
  .form-signin .form-signin-heading,
  .form-signin .checkbox {
    margin-bottom: 10px;
  }
  .form-signin .checkbox {
    font-weight: normal;
  }
  .form-signin .form-control {
    position: relative;
    height: auto;
    -webkit-box-sizing: border-box;
       -moz-box-sizing: border-box;
            box-sizing: border-box;
    padding: 10px;
    font-size: 16px;
  }
  .form-signin .form-control:focus {
    z-index: 2;
  }
  .form-signin input[type="email"] {
    margin-bottom: -1px;
    border-bottom-right-radius: 0;
    border-bottom-left-radius: 0;
  }
  .form-signin input[type="password"] {
    margin-bottom: 10px;
    border-top-left-radius: 0;
    border-top-right-radius: 0;
  }
|]

toggleButton :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Bool -> Text -> Text -> Text -> m (Dynamic t Bool)
toggleButton b0 k t1 t2 = elAttr "div" ("class" =: "btn-grp" <> "style" =: "overflow: auto") $ do
  rec short <- buttonDynAttr selAttrA $ do
        _ <- dyn $ fmap (\sel' -> if sel' then icon "check" else return ()) sel
        text t1
      long <- buttonDynAttr selAttrB $ do
        _ <- dyn $ fmap (\sel' -> if not sel' then icon "check" else return ()) sel
        text t2
      sel <- holdDyn b0 $ leftmost [fmap (const True) short, fmap (const False) long]
      let baseAttr :: Text -> Map Text Text
          baseAttr ksel = "class" =: ("btn " <> k <> " " <> ksel) <> "type" =: "button" <> "style" =: "width: 50%;"
          selAttrA = fmap (\sel' -> if sel' then baseAttr "btn-primary" else baseAttr "") sel
          selAttrB = fmap (\sel' -> if not sel' then baseAttr "btn-primary" else baseAttr "") sel
  return sel

toggleButtonStrip :: (Ord k, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Text -> k -> Map k Text -> m (Dynamic t k)
toggleButtonStrip k s0 labelMap = divClass "btn-grp" $ do
  rec selection <- holdDyn s0 changeE
      let baseAttr ksel = "class" =: ("btn " <> k <> " " <> ksel) <> "type" =: "button" <> "style" =: ("width: " <> (T.pack $ show (100 / fromIntegral (Map.size labelMap) :: Double)) <> "%;")
      changeE <- selectViewListWithKey_ selection (constDyn labelMap) $ \_ labelDyn isSelected -> do
        let attr = ffor isSelected $ \case
              True -> baseAttr "btn-primary"
              False -> baseAttr ""
        buttonDynAttr attr $ do
           dynIcon $ (ffor isSelected (\case True -> "check"; False -> ""))
           dynText labelDyn
  return selection

dayInputMini :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Day -> m (Dynamic t Day)
dayInputMini d0 = do
  let showDate = T.pack . formatTime defaultTimeLocale "%b %e, %Y"
  rec (e', attrs) <- elAttr' "div" ("class" =: "input-group pointer") $ do
        elClass "span" "input-group-addon" $ icon "clock-o"
        _ <- inputElement $ def
          & initialAttributes .~ ("class" =: "form-control" <> "readonly" =: "" <> "style" =: "cursor: pointer; background-color: #fff;")
          & inputElementConfig_setValue .~ fmap showDate date
          & inputElementConfig_initialValue .~ showDate d0
        isOpen <- holdDyn False $ leftmost [fmap (const False) date, fmap (const True) (domEvent Click e'), fmap (const False) close]
        return $ fmap (\x -> if x then "class" =: "dropdown-menu" <> "style" =: "width: auto; position: absolute; display: block;" else "class" =: "dropdown-menu") isOpen
      (date, close) <- elAttr "div" ("style" =: "position: relative; height: 0px; width: 0px") . elDynAttr "div" attrs $ do
        d <- dayInput d0
        return (updated d, never)
  holdDyn d0 date

checkButton :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Bool -> Text -> Text -> Text -> m (Dynamic t Bool)
checkButton b0 active inactive txt = do
  rec e <- buttonDynAttr toggleClass $ do
        elDynAttr "i" iconClass $ return ()
        text $ " " <> txt
      selected <- toggle b0 e
      let toggleClass = fmap (\s -> "type" =: "button" <> "class" =: if s then active else inactive) selected
          iconClass = fmap (\s -> "class" =: if s then "fa fa-check-square-o fa-fw" else "fa fa-square-o fa-fw") selected
  return selected

sortableTable
  :: (Ord k, Ord sv, Eq sk, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js m)
  => Dynamic t (Map k v)
  -- ^ The data set
  -> Dynamic t (Map Text Text)
  -- ^ The table attributes
  -> [sk]
  -- ^ The columns to display
  -> SortKey sk
  -- ^ Default sort key
  -> (sk -> v -> sv)
  -- ^ Key extractor (used for sorting)
  -> (k -> Dynamic t v -> m (Dynamic t (Map Text Text)))
  -- ^ Each row (tr) can have different attributes based on the state of the data
  -> (sk -> Either (m ()) Text)
  -- ^ How to render the header element - Left renderFunc | Right title (title is rendered with up/down arrows as appropriate)
  -> (sk -> Dynamic t v -> m ())
  -- ^ How to render the row. column sort key -> Dynamic val -> renderFunc
  -> (sk -> Dynamic t v -> m (Dynamic t (Map Text Text)))
  -- ^ Each row element (td) can have different attributes based on the state of the data
  -> Bool
  -- ^ Will the sorting will be done on the server itself?
  -> m (Dynamic t (SortKey sk))
sortableTable dynVals dynAttrs cols defaultSort extractKey rowAttrs mkHeaderElem mkRowElem rowElemAttrs serverSort = do
  let classAttrs = "class"=:"table col-md-12 table-bordered table-striped table-condensed cf tablesorter tablesorter-default"
  dynSortKey' <- elDynAttr "table" (Map.union classAttrs <$> dynAttrs) $ do
    dynSortKey <- elAttr "thead" ("class" =: "cf table-header") $
      elAttr "tr" ("role"=:"row" <> "class"=:"tablesorter-headerRow") $
        prerender (mkStaticHeader >> return (constDyn defaultSort)) $ sortableListHeader cols defaultSort mkHeaderElem'

    -- For server side sorting we simply display all list elements without modification
    el "tbody" $ do
      if serverSort
        then do
          -- `listWithKey` is buggy, we simply use dyn (performance hit isn't that big because the server replaces most rows anyways)
          -- listWithKey dynVals mkRow
          void $ dyn $ fmap (\(vs,sk) -> sequence_ $ map snd $ (sortOn fst) $ map (\(k,v) -> (fmap (`extractKey` v) sk, mkRow k (constDyn v))) $ Map.toList vs) $ zipDynWith (,) dynVals dynSortKey
        else
          -- For client side sorting, we use sortableListWithKey
          void $ sortableListWithKey dynVals dynSortKey extractKey mkRow

    return dynSortKey

  prerender mkSpinner blank
  return dynSortKey'

  where
    mkSpinner = el "center" $ elClass "i" "fa fa-gear fa-spin fa-4x shadow1 spinner" blank
    mkHeaderElem' sk dynSortKey = case mkHeaderElem sk of
      Left renderHeaderElem -> renderHeaderElem >> return never
      Right title -> do
        let d = ffor dynSortKey $ \x -> "class" =: (if x == Asc sk then "fa fa-fw fa-sort-asc" else if x == Desc sk then "fa fa-fw fa-sort-desc" else "fa fa-fw fa-sort")
        fmap (domEvent Click . fst) $ elAttr' "th" ("class"=:"tablesorter-header tablesorter-headerUnSorted" <> "role"=:"columnheader" <> "style"=:"-webkit-user-select: none; cursor: pointer;") $ do
            divClass "tablesorter-header-inner" $ do
              text title
              elDynAttr "i" d $ return ()
    mkRow k dynVal = do
      d <- fmap (<> "role" =: "row") <$> rowAttrs k dynVal
      elDynAttr "tr" d $ forM cols $ \sk -> do
        de <- rowElemAttrs sk dynVal
        elDynAttr "td" de $ mkRowElem sk dynVal
    mkStaticHeader = forM_ cols $ \col -> do
        case mkHeaderElem col of
          Left m -> m
          Right title -> void $ elAttr "th" ("class"=:"tablesorter-header tablesorter-headerUnSorted" <> "role"=:"columnheader" <> "style"=:"-webkit-user-select: none; cursor: pointer;") $ text title

-- Key to sort a table
data SortKey a = Asc a | Desc a
  deriving (Eq, Functor, Show, Read)

instance Ord a => Ord (SortKey a) where
  compare (Asc a) (Asc b) = compare a b
  compare (Desc a) (Desc b) = compare b a
  compare (Asc _) (Desc _) = LT
  compare (Desc _) (Asc _) = GT

-- | A Dynamic list which can be sorted
sortableListWithKey
  :: forall t k v m a sv sk. (Ord k, Ord sv, DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Dynamic t (Map k v)
  -> Dynamic t (SortKey sk)
  -> (sk -> v -> sv)
  -> (k -> Dynamic t v -> m a)
  -> m (Dynamic t (Map k a))
sortableListWithKey dynVals dynSortKey extractKey mkChild =
  fmap (Map.mapKeys snd) <$> listWithKey vals (mkChild . snd)
  where
    addSortKey valMap sortKey = Map.fromList $ map (\(k,v) -> ((fmap (`extractKey` v) sortKey, k), v)) $ Map.toList valMap
    vals = zipDynWith addSortKey dynVals dynSortKey

-- | Like `sortableListWithKey` but allows filtering with a search key
sortableSearchableListWithKey
  :: forall t k v m a sv sk. (Ord k, Ord sv, DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Dynamic t (Map k v)
  -> Dynamic t (SortKey sk)
  -> (sk -> v -> sv)
  -> Dynamic t Text
  -> (v -> Text)
  -> (k -> Dynamic t v -> m a)
  -> m (Dynamic t (Map k a))
sortableSearchableListWithKey dynVals dynSortKey extractKey dynSearch toString =
  sortableListWithKey (zipDynWith matchingSearch dynSearch dynVals) dynSortKey extractKey
  where
    matchingSearch search = Map.filter (hasText search . toString)
    hasText "" _ = True
    hasText search s = T.count (T.toLower search) (T.toLower s) > 0

-- | The header of a dynamic table. This can be used, for example, to create clickable headers to sort the table
sortableListHeader
  :: forall t m sk. (Eq sk, DomBuilder t m, MonadFix m, MonadHold t m)
  => [sk]
  -> SortKey sk
  -> (sk -> Dynamic t (SortKey sk) -> m (Event t ()))
  -> m (Dynamic t (SortKey sk))
sortableListHeader cols defaultSort mkHeaderElem = do
  rec
    evtSortKey <- leftmost <$> mapM (mkHeaderElem' dynSortKey) cols
    dynSortKey <- foldDyn changeSort defaultSort evtSortKey
  return dynSortKey
  where
    mkHeaderElem' dynSortKey sk = do
      evt <- mkHeaderElem sk dynSortKey
      return $ sk <$ evt
    changeSort a (Desc b) | a == b = Asc b
    changeSort a (Asc b) | a == b = Desc b
    changeSort a _ = fmap (const a) defaultSort


-- | Helper function for building the row of a dynamic table from the same `extractKey` function used for sorting the table
--   Will usually be combined with either `sortableSearchableListWithKey` or `sortableListWithKey`, for example -
--   `sortableListWithKey dynData dynSortKey extractKey $ sortableListRow columns extractKey mkRowElem`
sortableListRow
  :: forall t v m a sv sk. DomBuilder t m
  => [sk]
  -> (sk -> v -> sv)
  -> (sk -> Dynamic t sv -> m a)
  -> Dynamic t v -> m [a]
sortableListRow cols extractKey mkRowElem dynVal = mapM (\sk -> mkRowElem sk (fmap (extractKey sk) dynVal)) cols
