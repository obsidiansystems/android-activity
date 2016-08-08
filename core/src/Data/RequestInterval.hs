{-# LANGUAGE TemplateHaskell, MultiWayIf #-}
module Data.RequestInterval where
import Data.Min
import Data.Semigroup hiding (Min)
import qualified Focus.AppendMap as Map
import Focus.AppendMap (AppendMap)
import Data.Time
import Focus.Request
import Control.Lens (makeLenses)

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
             in Map.insert k v (Map.union mOld' mNew) -- put everything back together

makeJson ''RequestInterval
makeLenses ''RequestInterval
