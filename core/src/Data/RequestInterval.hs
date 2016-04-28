{-# LANGUAGE TemplateHaskell #-}
module Data.RequestInterval where
import Data.Min
import Data.Semigroup hiding (Min)
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

makeJson ''RequestInterval
makeLenses ''RequestInterval
