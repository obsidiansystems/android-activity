{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
-- | Various functions for making dealing with Groundhog more pleasant
module Focus.Backend.DB.Groundhog ( module Focus.Backend.DB.Groundhog
                                  , module Database.Groundhog
                                  , module Database.Groundhog.Core
                                  , module Database.Groundhog.TH
                                  ) where

import Focus.TH

import Database.Groundhog.Postgresql as DB
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Core as DB
import Database.Groundhog.TH
import Database.Groundhog.TH.Settings
import Language.Haskell.TH
import Data.Char
import Data.Monoid
import Control.Lens
import Control.Monad.Loops
import Data.List
import Data.Time.Clock
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State

import Debug.Trace

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
    (x, remainder) <- fromPersistValues pvs
    case remainder of
      [] -> return ()
      _ -> trace ("popAllRows: fromPersistValues left " <> show remainder <> " remaining out of " <> show pvs) $ return ()
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

instance PersistBackend m => PersistBackend (ReaderT r m) where --TODO: Abstract this newtype-wrapper-monad-stack stuff
  type PhantomDb (ReaderT r m) = PhantomDb m
  insert = lift . DB.insert
  insert_ = lift . DB.insert_
  insertBy u v = lift $ DB.insertBy u v
  insertByAll = lift . DB.insertByAll
  replace k v = lift $ DB.replace k v
  replaceBy u v = lift $ DB.replaceBy u v
  select = lift . DB.select
  selectAll = lift DB.selectAll
  get = lift . DB.get
  getBy = lift . DB.getBy
  update us c = lift $ DB.update us c
  delete = lift . DB.delete
  deleteBy = lift . DB.deleteBy
  deleteAll = lift . DB.deleteAll
  count = lift . DB.count
  countAll = lift . DB.countAll
  project p o = lift $ DB.project p o
  migrate v = mapStateT (lift) $ DB.migrate v
  executeRaw c q p = lift $ DB.executeRaw c q p
  queryRaw c q p f = do
    k <- ask
    lift $ DB.queryRaw c q p $ \rp -> runReaderT (f $ lift rp) k
  insertList = lift . DB.insertList
  getList = lift . DB.getList



