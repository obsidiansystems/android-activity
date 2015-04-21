{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, DeriveDataTypeable, DefaultSignatures, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module Focus.Subscription where

import Focus.Schema
import Focus.Request

import Control.Monad
import Data.Time
import Data.Text (Text)
import Web.Stripe.Plan (Plan)
import Web.Stripe.Utils (CustomerId)

data Subscription
  = Subscription { subscription_subscriptionPlan :: Maybe (Id SubscriptionPlan)
                 , subscription_trialExpiry :: Maybe UTCTime
                 , subscription_start :: UTCTime
                 , subscription_stripeCustomerId :: CustomerId
                 }
  deriving (Show, Read, Eq)

data SubscriptionPlan
  = SubscriptionPlan { subscriptionPlan_stripePlan :: Plan
                     , subscriptionPlan_description :: Text
                     }
  deriving (Show, Read, Eq)

instance HasId Subscription
instance HasId SubscriptionPlan

liftM concat $ mapM makeJson
  [ ''Subscription
  , ''SubscriptionPlan
  , ''CustomerId
  ]

