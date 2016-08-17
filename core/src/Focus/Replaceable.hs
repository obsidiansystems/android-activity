{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, TemplateHaskell, DeriveGeneric, DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts #-}
module Focus.Replaceable where

import Control.Lens.TH
import Data.Align
import Data.Aeson
import Data.Semigroup
import Data.Typeable
import GHC.Generics (Generic)
import Focus.Patch

newtype Replaceable t a = Replaceable { _unReplaceable :: t a }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, Functor, Foldable, Traversable, Align)

deriving instance (Semigroup (t a)) => Semigroup (Replaceable t a)
deriving instance (ToJSON (t a)) => ToJSON (Replaceable t a)
deriving instance (FromJSON (t a)) => FromJSON (Replaceable t a)

makeWrapped ''Replaceable

-- Either replace the contents entirely, or patch them
type ReplaceablePatch t a = Either (Replaceable t a) (Patch (t a))

instance (Patchable (t a)) => Patchable (Replaceable t a) where
  type Patch (Replaceable t a) = ReplaceablePatch t a

  -- The reason why this module exists - patching a ReplaceOuterPatch replaces *everything*
  patch (Left replacement) _original = replacement
  patch (Right p) (Replaceable original) = Replaceable $ patch p original

-- Create a patch to replace contents entirely
allPatchReplaceable :: (t a) -> ReplaceablePatch t a
allPatchReplaceable = Left . Replaceable

-- Create a normal patch for a Replaceable
somePatchReplaceable :: (Patch (t a)) -> ReplaceablePatch t a
somePatchReplaceable = Right
