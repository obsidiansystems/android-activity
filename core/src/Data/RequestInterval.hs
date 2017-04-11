{-# LANGUAGE CPP, MultiWayIf #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
module Data.RequestInterval where
import Data.Min
import Data.Semigroup hiding (Min)
import qualified Focus.AppendMap as Map
import Focus.AppendMap (AppendMap)
import Data.Time
#ifdef USE_TEMPLATE_HASKELL
import Focus.Request
import Control.Lens (makeLenses)
#else
import Control.Lens (Lens')
#endif
#if !defined(USE_TEMPLATE_HASKELL) || defined(DUMPING_SPLICES)
import Data.Aeson
import Focus.Request (HList(..))
#endif

data RequestInterval = RequestInterval { _requestInterval_point :: Min UTCTime
                                       , _requestInterval_count :: Int
                                       }
  deriving (Show, Read, Eq, Ord)

instance Semigroup RequestInterval where
  (RequestInterval p c) <> (RequestInterval p' c') = RequestInterval (p <> p') (c + c')

instance Monoid RequestInterval where
  mempty = RequestInterval Infinity 0
  mappend = (<>)

-- | Crop an AppendMap whose keys are ordered by time, and from which a UTCTime can be obtained, such that it fits the given request interval.
cropRequestInterval :: Ord k => (k -> UTCTime) -> RequestInterval -> AppendMap k v -> AppendMap k v
cropRequestInterval proj (RequestInterval mp c) m =
  let (mOld, mNew) =
        case mp of
          Min p -> Map.partitionWithKey (\k _ -> proj k < p) m -- TODO: this predicate is monotone, we should be able to do this in O(log n) somehow.
          Infinity -> (m, Map.empty)
      splitPoint = Map.size mOld - c -- we wish to retain c of the messages in mOld, starting from the end, so this is the index of the first message we're keeping
  in if | splitPoint <= 0 -> m -- we're not over the requested count of previous messages, so leave it alone
        | c == 0 -> mNew -- we don't want any messages older than the point
        | otherwise ->
            let (k,v) = Map.elemAt splitPoint mOld -- find the first message we want to keep, and its key
                (_,mOld') = Map.split k mOld -- get the rest of the messages we want to keep
             in Map.insert k v (Map.unionWith const mOld' mNew) -- put everything back together

#ifdef USE_TEMPLATE_HASKELL
makeJson ''RequestInterval
makeLenses ''RequestInterval
#else
instance ToJSON RequestInterval where
  toJSON r_a3prn
    = case r_a3prn of {
        RequestInterval f_a3pro f_a3prp
          -> toJSON
               ("RequestInterval" :: String,
              toJSON (HCons f_a3pro (HCons f_a3prp HNil))) }
instance FromJSON RequestInterval where
  parseJSON v_a3prs
    = do { (tag'_a3prt, v'_a3pru) <- parseJSON v_a3prs;
           case tag'_a3prt :: String of
             "RequestInterval"
               -> do { HCons f_a3prx (HCons f_a3pry HNil) <- parseJSON v'_a3pru;
                       return (RequestInterval f_a3prx f_a3pry) }
             _ -> fail "invalid message" }

requestInterval_point :: Lens' RequestInterval (Min UTCTime)
requestInterval_point f (RequestInterval a b) = (\a' -> RequestInterval a' b) <$> f a
{-# INLINE requestInterval_point #-}
requestInterval_count :: Lens' RequestInterval Int
requestInterval_count f (RequestInterval a b) = (\b' -> RequestInterval a b') <$> f b
{-# INLINE requestInterval_count #-}
#endif
