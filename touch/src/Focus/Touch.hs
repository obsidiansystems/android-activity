{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Focus.Touch where
import Control.Monad.IO.Class
import Data.Align
import Data.Default
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.These
import Data.Time
import Reflex.Dom

mkTouchStart :: MonadIO m => TouchEventResult -> m TouchEventStage
mkTouchStart r = liftIO $ do
  t <- getCurrentTime
  return $ TouchEventStage_Start t r

mkTouchEnd :: MonadIO m => TouchEventResult -> m TouchEventStage
mkTouchEnd r = liftIO $ do
  t <- getCurrentTime
  return $ TouchEventStage_End t r

-- | Creates a map of touch results keyed on the identifier field
--   The identifier field contains an ID that can be used to associate
--   touches related to a single, uninterrupted touch sequence or gesture
touchResultsById :: [TouchResult] -> Map Word (NonEmpty TouchResult)
touchResultsById = Map.fromListWith (<>) . fmap (\tr -> (_touchResult_identifier tr, tr :| []))

changedTouches :: TouchEventResult -> Map Word (NonEmpty TouchResult)
changedTouches = touchResultsById . _touchEventResult_changedTouches

data TouchEventStage = TouchEventStage_Start UTCTime TouchEventResult
                     | TouchEventStage_Move TouchEventResult
                     | TouchEventStage_End UTCTime TouchEventResult
                     | TouchEventStage_Cancel TouchEventResult
                     deriving (Show, Eq)

data Swipe = Swipe
  { _swipe_direction :: Float
  , _swipe_distance :: Float
  }
  deriving (Show, Eq)

data Gesture = Gesture_Tap
             | Gesture_LongPress
             | Gesture_Drag Swipe
             | Gesture_Swipe  NominalDiffTime Swipe
             deriving (Eq, Show)

-- | 'TouchResult's for a touch that hasn't ended or been canceled yet
data UnfinishedGesture = UnfinishedGesture UTCTime (NonEmpty TouchResult)
  deriving (Eq, Show)

-- | Sensitivity parameters for interpreting touch events
data TouchConfig = TouchConfig
  { _touchConfig_longPress :: NominalDiffTime
  -- ^ How long before a tap is considered a long press
  , _touchConfig_minimumSwipeDistance :: Float
  -- ^ How far a touch must travel (in pixels) before being considered a swipe
  }

instance Default TouchConfig where
  def = TouchConfig
    { _touchConfig_longPress = 0.5
    , _touchConfig_minimumSwipeDistance = 50
    }

-- | Produces gestures from touch events:
--   Currently we only recognize three gestures (see 'Gesture')
--   and we don't use anything but the start and end touch events
resolveTouchEvents :: TouchConfig
                   -> TouchEventStage
                   -> Map Word UnfinishedGesture
                   -> (Map Word UnfinishedGesture, [Gesture])
resolveTouchEvents cfg te state = case te of
  TouchEventStage_Start t r -> (Map.union state (UnfinishedGesture t <$> changedTouches r), [])
  TouchEventStage_Cancel r -> touchAlign state r $ \_ _ -> (Nothing, Nothing)
  TouchEventStage_Move r -> touchAlign state r $ \(UnfinishedGesture t0 a) b ->
    let p1 = coords $ NonEmpty.head a
        p2 = coords $ NonEmpty.last b
        dist = distance p1 p2
        deg = angle p1 p2
    in (Just $ UnfinishedGesture t0 $ a <> b, Just $ Gesture_Drag $ Swipe { _swipe_direction = deg, _swipe_distance = dist })
  TouchEventStage_End t1 r -> touchAlign state r $ \(UnfinishedGesture t0 a) end ->
    let p1 = coords $ NonEmpty.head a
        p2 = coords $ NonEmpty.last end
        dist = distance p1 p2
        deg = angle p1 p2
    in if | dist < minDist && diffUTCTime t1 t0 > longPress -> (Nothing, Just Gesture_LongPress)
          | dist < minDist -> (Nothing, Just Gesture_Tap)
          | otherwise -> (,) Nothing $ Just $ Gesture_Swipe (diffUTCTime t1 t0) $ Swipe
            { _swipe_distance = dist
            , _swipe_direction = deg
            }
  where
    touchAlign :: Map Word UnfinishedGesture
               -> TouchEventResult
               -> (UnfinishedGesture -> NonEmpty TouchResult -> (Maybe UnfinishedGesture, Maybe Gesture))
               -> (Map Word UnfinishedGesture, [Gesture])
    touchAlign s r f =
      let r' = (\x -> alignWith x s $ changedTouches r) $ \case
                This a -> (Just a, Nothing) -- Touches for this identifier have not been modified
                These a b -> f a b
                That _ -> (Nothing, Nothing) -- Received a touch update for an unstarted touch (shouldn't happen)
      in (Map.mapMaybe fst r', Map.elems $ Map.mapMaybe snd r')
    minDist = _touchConfig_minimumSwipeDistance cfg
    longPress = _touchConfig_longPress cfg

distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^(2::Int) + (y2 - y1)^(2::Int))

-- | (x, y) coordinates of a TouchResult
coords :: Floating a => TouchResult -> (a, a)
coords tr = (fromIntegral $ _touchResult_clientX tr, fromIntegral $ _touchResult_clientY tr)

-- | Angle in degrees
--          3π/2
--            |
--        π —   — 0
--            |
--           π/2
angle :: RealFloat a => (a, a) -> (a, a) -> a
angle (x1, y1) (x2, y2) = atan2 (y2 - y1) (x2 - x1)
