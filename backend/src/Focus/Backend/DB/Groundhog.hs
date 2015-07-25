{-# LANGUAGE TemplateHaskell #-}
-- | Various functions for making dealing with Groundhog more pleasant
module Focus.Backend.DB.Groundhog ( module Focus.Backend.DB.Groundhog
                                  , module Database.Groundhog
                                  , module Database.Groundhog.Core
                                  , module Database.Groundhog.TH
                                  ) where

import Focus.TH

import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.TH
import Database.Groundhog.TH.Settings
import Language.Haskell.TH
import Data.Char
import Data.Monoid
import Control.Lens
import Control.Monad.Loops
import Data.List
import Data.Time.Clock

-- | Run Database.Groundhog.TH.mkPersist with Focus-specific defaults
--
-- Record field names are expected to look like _dataTypeName_fieldName
mkFocusPersist :: Maybe String -> PersistDefinitions -> Q [Dec]
mkFocusPersist migrationFunctionName = mkPersist $ defaultCodegenConfig
  { migrationFunction = migrationFunctionName
  , namingStyle =
    let ns = namingStyle defaultCodegenConfig
        dropPrefix p x = if p `isPrefixOf` x then drop (length p) x else error $ "mkFocusPersist: dropPrefix: expected string with prefix " <> show p <> ", got string " <> show x
    in ns { mkDbFieldName = \typeName conName conPos fieldName fieldPos ->
             mkDbFieldName ns typeName conName conPos (dropPrefix ("_" <> (_head %~ toLower) typeName <> "_") fieldName) fieldPos
          , mkExprFieldName = \typeName conName conPos fieldName fieldPos ->
             mkExprFieldName ns typeName conName conPos (dropPrefix "_" fieldName) fieldPos
          , mkExprSelectorName  = \typeName conName fieldName fieldPos ->
             mkExprSelectorName ns typeName conName (dropPrefix "_" fieldName) fieldPos
          }
  }

-- | Apply the given function to field names before building Groundhog names based on them
prefilterFieldsNamingStyle :: (String -> String) -> NamingStyle -> NamingStyle
prefilterFieldsNamingStyle f ns = ns
  { mkDbFieldName = \typeName conName conPos fieldName fieldPos ->
      mkDbFieldName ns typeName conName conPos (f fieldName) fieldPos
  , mkExprFieldName = \typeName conName conPos fieldName fieldPos ->
      mkExprFieldName ns typeName conName conPos (f fieldName) fieldPos
  }

popAllRows :: (PersistBackend m, PersistField b) => m (Maybe [PersistValue]) -> m [b]
popAllRows rp = do
  whileJust rp $ \pvs -> do
    (x, []) <- fromPersistValues pvs
    return x

getTransactionTime :: PersistBackend m => m UTCTime
getTransactionTime = do
  [t] <- queryRaw False "select current_timestamp at time zone 'UTC'" [] popAllRows
  return t

makePersistFieldNewtype :: Name -> Q [Dec]
makePersistFieldNewtype t = do
  TyConI (NewtypeD _ _ _ con _) <- reify t
  let c = conName con
  xName <- newName "x"
  [d| instance PersistField $(conT t) where
        persistName _ = $(stringE $ nameBase t)
        toPersistValues $(conP c [varP xName]) = toPersistValues $(varE xName)
        fromPersistValues pv = do
          (x, pv') <- fromPersistValues pv
          return ($(conE c) x, pv')
        dbType p (~($(conP c [varP xName]))) = dbType p $(varE xName)
    |]
