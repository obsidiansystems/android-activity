{-# LANGUAGE RecursiveDo, RankNTypes, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TemplateHaskell, QuasiQuotes, LambdaCase, OverloadedStrings #-}
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

import Safe
import Control.Monad
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

bootstrapCDN :: MonadWidget t m => m ()
bootstrapCDN = elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css") $ return ()

button :: MonadWidget t m => Text -> m (Event t ())
button s = buttonClass "btn btn-primary" s

buttonClass :: MonadWidget t m => Text -> Text -> m (Event t ())
buttonClass k s = button' k $ text s

buttonActiveClass :: MonadWidget t m => Text -> Dynamic t Bool -> Text -> m (Event t ())
buttonActiveClass k actD s = buttonActive' k actD (text s)

button' :: MonadWidget t m => Text -> m a -> m (Event t ())
button' k w = buttonDynAttr (constDyn $ "class" =: k <> "type" =: "button") w

buttonActive' :: MonadWidget t m => Text -> Dynamic t Bool -> m a -> m (Event t ())
buttonActive' k actD w = do attrs <- forDyn actD $ \active -> "class" =: k <> "type" =: "button" <> if active then mempty else "disabled" =: "1"
                            buttonDynAttr attrs w

buttonDynAttr :: MonadWidget t m => Dynamic t (Map Text Text) -> m a -> m (Event t ())
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

data NumberInput t a = NumberInput { _numberInput_value :: Dynamic t (Maybe a)
                                   , _numberInput_textInput :: TextInput t }

instance HasValue (NumberInput t a) where
  type Value (NumberInput t a) = Dynamic t (Maybe a)
  value = _numberInput_value

numberInput :: (Num a, MonadWidget t m) => Text -> m (NumberInput t a)
numberInput initial =
  do ti <- textInput $ def & textInputConfig_inputType .~ "number"
                           & textInputConfig_initialValue .~ initial
                           & attributes .~ constDyn (Map.fromList [("class", "form-control")])
     v <- mapDyn (fmap fromInteger . readMay . T.unpack) (value ti)
     return (NumberInput v ti)

dayInput :: MonadWidget t m => Day -> m (Dynamic t Day)
dayInput d0 = do
  let (year0, month0, _ {- dayOfMonth0 -} ) = toGregorian d0
  rec visibleMonth <- foldDyn ($) (year0, intToMonth month0) navigate'
      (navigate', dayClicked') <- do
        (prevButton, nextButton) <- divClass "text-center" $ do
          p <- linkClass "<<" "btn btn-sm pull-left"
          elAttr "span" ("style" =: "position:relative; top: 10px") $ dynText =<< mapDyn (\(y, m) -> T.pack (show m) <> " " <> T.pack (show y)) visibleMonth
          n <- linkClass ">>" "btn btn-sm pull-right"
          return (p, n)
        mc :: Event t (Event t Int) <- dyn <=< forDyn visibleMonth $ \(y, m) -> do
          mDayOfMonth <- forDyn day $ \d ->
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
      day <- holdDyn d0 $ attachDynWith (\(y, m) d -> fromGregorian y (monthToInt m) d) visibleMonth dayClicked'
  return day

monthCal :: forall t m. MonadWidget t m => Integer -> Month -> Dynamic t (Maybe Int) -> m (Event t Int)
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
            active <- forDyn sel $ \s -> "class" =: ("btn btn-xs" <> if s == n && isJust s then " btn-primary" else "")
            (e, _) <- elAttr' "td" attrs $ elDynAttr "a" active $ text $ T.pack $ maybe "" show n
            return $ fmap (const n) $ domEvent Click e
  let selectionMade = leftmost $ concat click
  return $ fmapMaybe id selectionMade

timeInput :: forall t m. MonadWidget t m => TimeOfDay -> m (Dynamic t TimeOfDay)
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
  milHour <- combineDyn (\h m -> case m of
                                      AM -> if h == 12 then 0 else h
                                      PM -> if h == 12 then 12 else h + 12) hour meridian
  tod <- liftM (fmapMaybe id . updated) $ combineDyn (\h m -> makeTimeOfDayValid h m 0) milHour minute
  holdDyn t0 tod

mainlandUSTimeZoneMap :: (MonadWidget t m, HasJS x m) => m (Map Text TimeZoneSeries)
mainlandUSTimeZoneMap = do
  kvs <- forM ["Eastern", "Central", "Mountain", "Pacific"] $ \n ->
            do s <- getTimeZoneSeries ("US/" <> n)
               return (n,s)
  return (Map.fromList [(n,s) | (n,Just s) <- kvs])

mainlandUSTimeInput :: (HasJS x m, MonadWidget t m) => Map Text TimeZoneSeries -> UTCTime -> m (Dynamic t UTCTime)
mainlandUSTimeInput tzMap t0 = 
  utcTimeInputMini (tzMap Map.! "Eastern") (mainlandUSTimeZone tzMap def) t0

mainlandUSTimeZone :: (HasJS x m, MonadWidget t m) => Map Text TimeZoneSeries -> DropdownConfig t Text -> m (Dynamic t TimeZoneSeries)
mainlandUSTimeZone tzMap _ {- cfg -} = do
  let labelMap, valueMap :: Map Int Text
      labelMap = 0 =: "PT"      <> 1 =: "MT"       <> 2 =: "CT"      <> 3 =: "ET"
      valueMap = 0 =: "Pacific" <> 1 =: "Mountain" <> 2 =: "Central" <> 3 =: "Eastern"
  selection <- mapDyn (valueMap Map.!) =<< toggleButtonStrip "btn-xs" 3 labelMap
  mapDyn (tzMap Map.!) selection

utcTimeInputMini :: (HasJS x m, MonadWidget t m) => TimeZoneSeries -> m (Dynamic t TimeZoneSeries) -> UTCTime -> m (Dynamic t UTCTime)
utcTimeInputMini tz0 tzWidget t = do
  rec timeShown <- combineDyn (\tz t' -> showDateTime' tz t') tzD timeD
      (e', attrs) <- elAttr' "div" ("class" =: "input-group pointer") $ do
        elClass "span" "input-group-addon" $ icon "clock-o"
        _ <- textInput $ def & attributes .~ (constDyn $ "class" =: "form-control" <> "disabled" =: "" <> "style" =: "cursor: pointer; background-color: #fff;")
                             & textInputConfig_setValue .~ updated timeShown
                             & textInputConfig_initialValue .~ (showDateTime' tz0) t
        isOpen <- holdDyn False $ leftmost [fmap (const True) (domEvent Click e'), fmap (const False) close]
        attrs' :: Dynamic t (Map Text Text) <- mapDyn (\x -> if x then "class" =: "dropdown-menu"
                                                                        <> "style" =: "width: auto; position: absolute; display: block;"
                                                                      else "class" =: "dropdown-menu")
                                                          isOpen
        return attrs'
      (timeD, tzD, close) <- elDynAttr "div" attrs $ do
        close' <- liftM (domEvent Click . fst) $ elAttr' "div" ("class" =: "modal-background" <> "style" =: "position:fixed; background-color: rgba(0,0,0,0)") $ return ()
        elAttr "div" ("style" =: "z-index:20; position:relative") $ do
          rec ltD <- divClass "form-inline text-center" $ do
                t' <- timeInput (localTimeOfDay $ utcToLocalTime' tz0 t)
                ltD' <- combineDyn (\day t'' -> LocalTime day t'') d t'
                return ltD'
              tzD <- elAttr "div" ("style" =: "padding-top: 5px") $ tzWidget
              d <- dayInput (localDay $ utcToLocalTime' tz0 t)
              timeD <- combineDyn (\tz lt -> localTimeToUTC' tz lt) tzD ltD
          return (timeD, tzD, close')
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

sortList :: forall t m k v a. (MonadWidget t m, Ord k) => Dynamic t (Map k v) -> Event t (Maybe k) -> (v -> Text) -> (Maybe (Int, k) -> Dynamic t v -> Dynamic t Bool -> m (Event t a)) -> m (Dynamic t (Maybe k))
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

sortListWithDeselectButton :: (Ord k, MonadWidget t m) => Text -> Dynamic t (Map k v) -> (v -> Text) -> (Maybe (Int, k) -> Dynamic t v -> Dynamic t Bool -> m (Event t a )) -> m (Dynamic t (Maybe k))
sortListWithDeselectButton buttonLabel items toString mkChild = do
  b <- button buttonLabel
  let deselect = fmap (const Nothing) b
  sortList items deselect toString mkChild

substringFilter :: Maybe Text -> Text -> Bool
substringFilter q s = case q of
                           Nothing -> True
                           Just "" -> True
                           Just q' -> T.count (T.toLower q') (T.toLower s) > 0

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

searchBox' :: MonadWidget t m => Text -> Text -> m (Dynamic t (Maybe Text))
searchBox' i p = do
  divClass "input-group" $ do
    elClass "span" "input-group-addon" $ icon i
    q <- liftM value (textInputWithPlaceholder p)
    forDyn q $ \v -> let x = T.strip v
                     in if x == "" then Nothing else Just x

searchBox :: MonadWidget t m => Text -> m (Dynamic t (Maybe Text))
searchBox i = searchBox' i "Search..."

readonlyInput :: forall t m. MonadWidget t m => Text -> Text -> Text -> Event t Text -> m (TextInput t)
readonlyInput inputType initial _ eSetVal = textInput $ def & textInputConfig_inputType .~ inputType
                                                            & textInputConfig_initialValue .~  initial
                                                            & setValue .~ eSetVal
                                                            & attributes .~ constDyn (Map.fromList [("class", "form-control"), ("readonly", "readonly")])


inputWithPlaceholder' :: forall t m. MonadWidget t m => Text -> Text -> Text -> Event t Text -> m (TextInput t)
inputWithPlaceholder' inputType initial p eSetVal = textInput $ def & textInputConfig_inputType .~ inputType
                                                                    & textInputConfig_initialValue .~  initial
                                                                    & setValue .~ eSetVal
                                                                    & attributes .~ constDyn (Map.fromList [("class", "form-control"), ("placeholder", p)])

inputWithPlaceholder :: forall t m. MonadWidget t m => Text -> Text -> m (TextInput t)
inputWithPlaceholder inputType p = inputWithPlaceholder' inputType "" p never

textInputWithPlaceholder :: forall t m. MonadWidget t m => Text -> m (TextInput t)
textInputWithPlaceholder p = inputWithPlaceholder "text" p

passwordInputWithPlaceholder :: forall t m. MonadWidget t m => Text -> m (TextInput t)
passwordInputWithPlaceholder p = inputWithPlaceholder "password" p

emailInputWithPlaceholder :: forall t m. MonadWidget t m => Text -> m (TextInput t)
emailInputWithPlaceholder p = inputWithPlaceholder "email" p

labelledInput :: forall t m a. MonadWidget t m => Text -> m a -> m a
labelledInput name content = do
  divClass "form-group" $ do
    elAttr "label" (Map.fromList [("for", ""), ("class", "col-sm-2 control-label")]) $ do
      text $ name
    divClass "col-sm-10" $ return =<< content

buttonWithIcon :: forall t m. MonadWidget t m => Text -> Text -> Text -> m (Event t ())
buttonWithIcon i s btnClass = button' btnClass $ do
  icon i
  text $ " " <> s

stamp :: MonadWidget t m => Text -> Text -> m ()
stamp k v = elAttr "span" ("class" =: ("stamp " <> k) <> stampDefaultStyle) $ text v

stamp' :: MonadWidget t m => Text -> Text -> m ()
stamp' k v = stampWidget k $ text v

stampWidget :: MonadWidget t m => Text -> m a -> m a
stampWidget k w = elAttr "span" ("class" =: ("stamp " <> k)) w

stampDefaultStyle :: Map Text Text
stampDefaultStyle = "style" =: "color: white; text-transform: uppercase; font-weight: bold; padding-left: 0.25em; padding-right: 0.25em; font-size: small; border-radius: 0.25em; box-shadow: 1px 1px 1px black; text-shadow: 1px 1px 1px black;"

tristateButton :: MonadWidget t m => Text -> Text -> Dynamic t (Maybe Bool) -> m (Event t ())
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

maybeBadge :: forall t m a. (MonadWidget t m, Show a) => Dynamic t (Maybe a) -> m (Event t ())
maybeBadge mInt = dyn $ ffor mInt $ maybe blank $ \n -> elAttr "span" (Map.singleton "class" "badge") $ text $ T.pack $ show n

checkboxWithLabel :: forall t m. MonadWidget t m => Text -> m (Checkbox t)
checkboxWithLabel l = checkboxWithLabelAttrs l Map.empty

checkboxWithLabelAttrs :: forall t m. MonadWidget t m => Text -> Map Text Text -> m (Checkbox t)
checkboxWithLabelAttrs l attrs = elAttr "label" attrs $ do
  c <- checkbox False def
  text $ " " <> l
  return c

progressBar :: MonadWidget t m => Text -> Dynamic t Double -> m ()
progressBar k w = divClass "progress" $ do
  attrs <- forDyn w $ \w' -> "class" =: ("progress-bar progress-bar-" <> k) <> "style" =: ("width: " <> T.pack (show w') <> "%")
  elDynAttr "div" attrs $ return ()

maybeDisplay :: forall t m a b. MonadWidget t m => a -> (Dynamic t a -> m (Dynamic t (Maybe b))) -> (a -> b -> m ()) -> m ()
maybeDisplay someId watcher child = do
  _ <- dyn =<< mapDyn (maybe (text "Loading...") $ child someId) =<< watcher (constDyn someId)
  return ()

jumbotron :: forall t m a. MonadWidget t m => Text -> Text -> m a -> m a
jumbotron t subtitle child = divClass "jumbotron" $ do
  elAttr "h1" (Map.singleton "class" "text-center") $ do
    text t
  elAttr "h1" (Map.singleton "class" "text-center") $ do
    el "small" $ text subtitle
  child

withLoginWorkflow'
  :: forall t m loginInfo newUser. (MonadWidget t m)
  => Bool -- ^ Whether to display sign-up form first
  -> (m (Event t (Maybe loginInfo), Event t (Workflow t m (Event t (Maybe loginInfo)))) ->
      m (Event t (Maybe loginInfo), Event t (Workflow t m (Event t (Maybe loginInfo)))))
  -- ^ wrapper widget
  -> Maybe loginInfo
  -- ^ initial login information
  -> m (Event t newUser, Event t ())
  -- ^ New Account (New User, return to signin)
  -> m (Event t (), Event t ())
  -- ^ Recover (Password Reset Requested, return to signin)
  -> m (Event t loginInfo, Event t ())
  -- ^ Login (Successful login request, return to signup)
  -> (loginInfo -> m (Event t ()))
  -- ^ Post-login, returns a logout event
  -> Workflow t m (Event t (Maybe loginInfo))
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
        return (fmap Just eLoginInfo, fmap f' eLoginInfo)
      f' x = Workflow $ do
        eLogout <- f x
        return (fmap (const Nothing) eLogout, fmap (const loginWorkflow') eLogout)
  in maybe loginWorkflow' f' li0

recoveryForm
  :: forall t m. (MonadWidget t m)
  => (Event t Email -> m (Event t ()))
  -- ^ Recover password request
  -> m (Event t (), Event t ())
recoveryForm requestPasswordResetEmail = elAttr "form" (Map.singleton "class" "form-signin") $ do
  signinLink <- elAttr "h3" (Map.singleton "class" "form-signin-heading") $ do
    text "Recover or "
    linkClass "sign in" "pointer"
  emailBox <- emailInputWithPlaceholder "Email address"
  submitButton <- linkClass "Recover" "btn btn-lg btn-primary btn-block"
  eReset <- requestPasswordResetEmail $ tag (current $ _textInput_value emailBox) (_link_clicked submitButton)
  el "div" $ elAttr "p" (Map.singleton "class" "text-center") $ dynText =<< holdDyn "" (fmap (const "An email with account recovery instructions has been sent.") eReset)
  return (eReset, (_link_clicked signinLink))

loginForm
  :: forall t m loginInfo. (MonadWidget t m)
  => (Event t (Email, Text) -> m (Event t (Maybe loginInfo)))
  -- ^ Login request
  -> m (Event t loginInfo, Event t ())
loginForm login = elAttr "form" (Map.singleton "class" "form-signin") $ do
  signupLink <- elAttr "h3" (Map.singleton "class" "form-signin-heading") $ do
    text "Sign in or "
    linkClass "sign up" "pointer"
  emailBox <- emailInputWithPlaceholder "Email address"
  passwordBox <- passwordInputWithPlaceholder "Password"
  submitButton <- linkClass "Sign in" "btn btn-lg btn-primary btn-block"
  let bCreds = pull $ do
        email <- sample $ current $ _textInput_value emailBox
        password <- sample $ current $ _textInput_value passwordBox
        return (email, password)
  let eEmailEnter = textInputGetEnter emailBox
      ePasswordEnter = textInputGetEnter passwordBox
  eLoginResult <- login $ tag bCreds $ leftmost [eEmailEnter, ePasswordEnter, _link_clicked submitButton]
  errorAttrs <- holdDyn ("style" =: "display: none") (fmap (\r' -> if isNothing r' then "class" =: "alert alert-warning" else "style" =: "display: none") eLoginResult)
  elDynAttr "div" errorAttrs $ do
    icon "warning"
    text " Invalid email address or password"
  let eLoginSuccess = fmapMaybe id eLoginResult
  return (eLoginSuccess, (_link_clicked signupLink))

newAccountForm
  :: MonadWidget t m
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
  :: MonadWidget t m
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
  :: MonadWidget t m
  => Signed PasswordResetToken
  -> (Event t (Signed PasswordResetToken, Text) -> m (Event t loginInfo))
  -> m (Event t loginInfo)
passwordReset token reset = elAttr "form" (Map.singleton "class" "form-signin") $ do
  pw <- passwordInputWithPlaceholder "Password"
  confirm <- passwordInputWithPlaceholder "Confirm Password"
  dPasswordConfirmed <- combineDyn (==) (_textInput_value pw) (_textInput_value confirm)
  dAttrs <- mapDyn (\c -> Map.singleton "class" $ "btn btn-lg btn-primary btn-block" <> if c then "" else " disabled") dPasswordConfirmed
  (submitButton, _) <- elDynAttr' "a" dAttrs $ text "Set Password"
  let enterPressed = gate (current dPasswordConfirmed) $ leftmost [textInputGetEnter pw, textInputGetEnter confirm]
      submit = leftmost [domEvent Click submitButton, enterPressed]
  loginInfo <- reset $ fmap (\p -> (token, p)) $ tag (current (_textInput_value pw)) submit
  --performEvent_ $ fmap (const $ liftIO $ windowHistoryPushState "/") loginInfo
  return loginInfo

styleTagSignin :: MonadWidget t m => m ()
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

toggleButton :: MonadWidget t m => Bool -> Text -> Text -> Text -> m (Dynamic t Bool)
toggleButton b0 k t1 t2 = elAttr "div" ("class" =: "btn-grp" <> "style" =: "overflow: auto") $ do
  rec short <- buttonDynAttr selAttrA $ do
        _ <- dyn =<< mapDyn (\sel' -> if sel' then icon "check" else return ()) sel
        text t1
      long <- buttonDynAttr selAttrB $ do
        _ <- dyn =<< mapDyn (\sel' -> if not sel' then icon "check" else return ()) sel
        text t2
      sel <- holdDyn b0 $ leftmost [fmap (const True) short, fmap (const False) long]
      let baseAttr :: Text -> Map Text Text
          baseAttr ksel = "class" =: ("btn " <> k <> " " <> ksel) <> "type" =: "button" <> "style" =: "width: 50%;"
      selAttrA <- mapDyn (\sel' -> if sel' then baseAttr "btn-primary" else baseAttr "") sel
      selAttrB <- mapDyn (\sel' -> if not sel' then baseAttr "btn-primary" else baseAttr "") sel
  return sel

toggleButtonStrip :: (Ord k, MonadWidget t m) => Text -> k -> Map k Text -> m (Dynamic t k)
toggleButtonStrip k s0 labelMap = divClass "btn-grp" $ do
  rec selection <- holdDyn s0 changeE
      let baseAttr ksel = "class" =: ("btn " <> k <> " " <> ksel) <> "type" =: "button" <> "style" =: ("width: " <> (T.pack $ show (100 / fromIntegral (Map.size labelMap) :: Double)) <> "%;")
      changeE <- selectViewListWithKey_ selection (constDyn labelMap) $ \_ labelDyn isSelected ->
                   do attr <- forDyn isSelected $ \case True -> baseAttr "btn-primary"; False -> baseAttr ""
                      buttonDynAttr attr $ do
                         dynIcon =<< (forDyn isSelected (\case True -> "check"; False -> ""))
                         dynText labelDyn
  return selection

dayInputMini :: MonadWidget t m => Day -> m (Dynamic t Day)
dayInputMini d0 = do
  let showDate = T.pack . formatTime defaultTimeLocale "%b %e, %Y"
  rec (e', attrs) <- elAttr' "div" ("class" =: "input-group pointer") $ do
        elClass "span" "input-group-addon" $ icon "clock-o"
        _ <- textInput $ def & attributes .~ (constDyn $ "class" =: "form-control" <> "disabled" =: "" <> "style" =: "cursor: pointer; background-color: #fff;")
                             & textInputConfig_setValue .~ fmap showDate date
                             & textInputConfig_initialValue .~ showDate d0
        isOpen <- holdDyn False $ leftmost [fmap (const False) date, fmap (const True) (domEvent Click e'), fmap (const False) close]
        attrs' :: Dynamic t (Map Text Text) <- mapDyn (\x -> if x then "class" =: "dropdown-menu" <> "style" =: "width: auto; position: absolute; display: block;" else "class" =: "dropdown-menu") isOpen
        return attrs'
      (date, close) <- elAttr "div" ("style" =: "position: relative; height: 0px; width: 0px") . elDynAttr "div" attrs $ do
        d <- dayInput d0
        return (updated d, never)
  holdDyn d0 date

checkButton :: MonadWidget t m => Bool -> Text -> Text -> Text -> m (Dynamic t Bool)
checkButton b0 active inactive txt = do
  rec e <- buttonDynAttr toggleClass $ do
        elDynAttr "i" iconClass $ return ()
        text $ " " <> txt
      selected <- toggle b0 e
      toggleClass <- mapDyn (\s -> "type" =: "button" <> "class" =: if s then active else inactive) selected
      iconClass <- mapDyn (\s -> "class" =: if s then "fa fa-check-square-o fa-fw" else "fa fa-square-o fa-fw") selected
  return selected


