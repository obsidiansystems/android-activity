{-# LANGUAGE TemplateHaskell #-}
module Data.RequestInterval where
import Data.Min
import Data.Monoid
import Data.Time
import Focus.Request
import Control.Lens (makeLenses)

data RequestInterval = RequestInterval { _requestInterval_point :: Min UTCTime
                                       , _requestInterval_count :: Int
                                       }
  deriving (Show, Read, Eq, Ord)

instance Monoid RequestInterval where
  mempty = RequestInterval Infinity 0
  mappend (RequestInterval p c) (RequestInterval p' c') = RequestInterval (p <> p') (c + c')

makeJson ''RequestInterval
makeLenses ''RequestInterval
