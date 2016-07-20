{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Focus.App where

import Data.Aeson
import Data.Align
import Data.Semigroup

import Focus.Patch
import Focus.Request

class ( ToJSON (ViewSelector app ()), FromJSON (ViewSelector app ())
      , Monoid (ViewSelector app ()), Semigroup (ViewSelector app ())
      , Monoid (ViewPatch app), Semigroup (ViewPatch app)
      , ToJSON (ViewPatch app), FromJSON (ViewPatch app)
      , Patchable (View app), Patch (View app) ~ ViewPatch app
      , Align (ViewSelector app)
      ) => HasView app where
  data View app
  data ViewPatch app
  type ViewSelector app :: * -> *
  emptyView :: View app
  cropView :: ViewSelector app a -> View app -> View app
  patchView :: ViewPatch app -> View app -> View app

class (Request (PublicRequest app), Request (PrivateRequest app)) => HasRequest app where
  data PublicRequest app :: * -> *
  data PrivateRequest app :: * -> *

