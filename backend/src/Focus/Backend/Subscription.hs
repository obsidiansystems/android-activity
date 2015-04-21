{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module Focus.Backend.Subscription where

import Focus.Backend.Schema.TH
import Focus.Schema
import Focus.Subscription

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.Groundhog
import Database.Groundhog.Generic
import Database.Groundhog.Core hiding (Proxy)
import Database.Groundhog.TH
import Data.Aeson.TH
import Control.Monad
import Data.Aeson
import Control.Applicative
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy

import Web.Stripe.Plan
import Web.Stripe.Utils (CustomerId (..), unCustomerId)

instance NeverNull PlanId

instance PrimitivePersistField PlanId where
  toPrimitivePersistValue p a = toPrimitivePersistValue p $ T.unpack $ unPlanId a
  fromPrimitivePersistValue p a = PlanId $ T.pack $ fromPrimitivePersistValue p a

instance PersistField PlanId where
  persistName _ = "PlanId"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField Amount where
  toPrimitivePersistValue p a = toPrimitivePersistValue p $ unAmount a
  fromPrimitivePersistValue p a = Amount $ fromPrimitivePersistValue p a

instance PersistField Amount where
  persistName _ = "Amount"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbReal False Nothing Nothing

instance PrimitivePersistField PlanInterval where
  toPrimitivePersistValue p a = toPrimitivePersistValue p $ show a
  fromPrimitivePersistValue p a = read $ fromPrimitivePersistValue p a

instance PersistField PlanInterval where
  persistName _ = "PlanInterval"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField Currency where
  toPrimitivePersistValue p a = toPrimitivePersistValue p $ show a
  fromPrimitivePersistValue p a = read $ fromPrimitivePersistValue p a

instance PersistField Currency where
  persistName _ = "Currency"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance NeverNull PlanTrialDays

instance PrimitivePersistField PlanTrialDays where
  toPrimitivePersistValue p a = toPrimitivePersistValue p $ unPlanTrialDays a
  fromPrimitivePersistValue p a = PlanTrialDays $ fromPrimitivePersistValue p a

instance PersistField PlanTrialDays where
  persistName _ = "PlanTrialDays"
  toPersistValues = primToPersistValue . unPlanTrialDays
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbInt32 False Nothing Nothing

instance NeverNull CustomerId

instance PrimitivePersistField CustomerId where
  toPrimitivePersistValue p a = toPrimitivePersistValue p $ T.unpack $ unCustomerId a
  fromPrimitivePersistValue p a = CustomerId $ T.pack $ fromPrimitivePersistValue p a

instance PersistField CustomerId where
  persistName _ = "CustomerId"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

