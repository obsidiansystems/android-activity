{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Focus.App where

import Data.Aeson
import Data.Semigroup
import Focus.Request

class ( Monoid (ViewSelector app), Semigroup (ViewSelector app)
      , ToJSON (ViewSelector app), FromJSON (ViewSelector app)
      , Monoid (ViewPatch app), Semigroup (ViewPatch app)
      , ToJSON (ViewPatch app), FromJSON (ViewPatch app)) => HasView app where
  data View app
  data ViewPatch app
  data ViewSelector app
  emptyView :: View app
  cropView :: ViewSelector app -> View app -> View app
  patchView :: ViewPatch app -> View app -> View app

class (Request (PublicRequest app), Request (PrivateRequest app)) => HasRequest app where
  data PublicRequest app :: * -> *
  data PrivateRequest app :: * -> *

