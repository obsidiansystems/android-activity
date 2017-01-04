{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Database.Groundhog.TH
import Database.Groundhog.TH.Settings
import Database.Groundhog.Expression
import Database.Groundhog.Generic.Sql
import Language.Haskell.TH
import Data.Char
import Control.Lens
import Control.Monad.Loops
import Data.List
import Data.Time.Clock
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State (StateT)
import Control.Monad.Trans.Maybe
import qualified Control.Monad.State as S
import qualified Control.Monad.State.Strict as Strict

import Debug.Trace

-- | Groundhog 'append' only works on String values. 'tappend' works on any 'IsString'
tappend :: (SqlDb db, ExpressionOf db r a c, ExpressionOf db r b c) => a -> b -> Expr db r c
tappend a b = mkExpr $ function "concat" [toExpr a, toExpr b]

-- | Run Database.Groundhog.TH.mkPersist with Focus-specific defaults
--
-- Record field names are expected to look like _dataTypeName_fieldName
mkFocusPersist :: Maybe String -> PersistDefinitions -> Q [Dec]
mkFocusPersist migrationFunctionName = mkPersist $ defaultCodegenConfig
  { migrationFunction = migrationFunctionName
  , namingStyle =
    let ns = namingStyle defaultCodegenConfig
        dropPrefix p x = if p `isPrefixOf` x then drop (length p) x else error $ "mkFocusPersist: dropPrefix: expected string with prefix " <> show p <> ", got string " <> show x
    in ns { mkDbFieldName = \typeName cN conPos fieldName fieldPos ->
             mkDbFieldName ns typeName cN conPos (dropPrefix ("_" <> (_head %~ toLower) typeName <> "_") fieldName) fieldPos
          , mkExprFieldName = \typeName cN conPos fieldName fieldPos ->
             mkExprFieldName ns typeName cN conPos (dropPrefix "_" fieldName) fieldPos
          , mkExprSelectorName  = \typeName cN fieldName fieldPos ->
             mkExprSelectorName ns typeName cN (dropPrefix "_" fieldName) fieldPos
          }
  }

-- | Apply the given function to field names before building Groundhog names based on them
prefilterFieldsNamingStyle :: (String -> String) -> NamingStyle -> NamingStyle
prefilterFieldsNamingStyle f ns = ns
  { mkDbFieldName = \typeName cN conPos fieldName fieldPos ->
      mkDbFieldName ns typeName cN conPos (f fieldName) fieldPos
  , mkExprFieldName = \typeName cN conPos fieldName fieldPos ->
      mkExprFieldName ns typeName cN conPos (f fieldName) fieldPos
  }

popAllRows :: (PersistBackend m, PersistField b) => m (Maybe [PersistValue]) -> m [b]
popAllRows rp = do
  whileJust rp $ \pvs -> do
    (x, remainder) <- fromPersistValues pvs
    case remainder of
      [] -> return ()
      _ -> trace ("popAllRows: fromPersistValues left " <> show remainder <> " remaining out of " <> show pvs) $ return ()
    return x

-- | Useful variant of mapAllRows for debugging raw queries. Prints the values obtained from the DB.
mapAllRows' :: MonadIO m => ([PersistValue] -> m a) -> RowPopper m -> m [a]
mapAllRows' f pop = go where
  go = do xs <- pop
          liftIO (print xs)
          maybe (return []) (f >=> \a -> liftM (a:) go) $ xs

getTransactionTime :: PersistBackend m => m UTCTime
getTransactionTime = do
  [t] <- queryRaw False "select current_timestamp at time zone 'UTC'" [] popAllRows
  return t

makePersistFieldNewtype :: Name -> Q [Dec]
makePersistFieldNewtype t = do
  TyConI (NewtypeD _ _ _ _ con _) <- reify t
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
  migrate v = S.mapStateT (lift) $ DB.migrate v
  executeRaw c q p = lift $ DB.executeRaw c q p
  queryRaw c q p f = do
    k <- ask
    lift $ DB.queryRaw c q p $ \rp -> runReaderT (f $ lift rp) k
  insertList = lift . DB.insertList
  getList = lift . DB.getList

instance PersistBackend m => PersistBackend (StateT s m) where
  type PhantomDb (StateT s m) = PhantomDb m
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
  migrate v = S.mapStateT (lift) $ DB.migrate v
  executeRaw c q p = lift $ DB.executeRaw c q p
  queryRaw c q p f = do
    k <- S.get
    (a, s) <- lift $ DB.queryRaw c q p $ \rp -> S.runStateT (f $ lift rp) k
    S.put s >> return a
  insertList = lift . DB.insertList
  getList = lift . DB.getList

instance PersistBackend m => PersistBackend (Strict.StateT s m) where
  type PhantomDb (Strict.StateT s m) = PhantomDb m
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
  migrate v = S.mapStateT (lift) $ DB.migrate v
  executeRaw c q p = lift $ DB.executeRaw c q p
  queryRaw c q p f = do
    k <- Strict.get
    (a, s) <- lift $ DB.queryRaw c q p $ \rp -> Strict.runStateT (f $ lift rp) k
    Strict.put s >> return a
  insertList = lift . DB.insertList
  getList = lift . DB.getList

instance PersistBackend m => PersistBackend (MaybeT m) where
  type PhantomDb (MaybeT m) = PhantomDb m
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
  migrate v = S.mapStateT (lift) $ DB.migrate v
  executeRaw c q p = lift $ DB.executeRaw c q p
  queryRaw c q p f = do
    ma <- lift $ DB.queryRaw c q p $ \rp -> runMaybeT (f $ lift rp)
    MaybeT (return ma)
  insertList = lift . DB.insertList
  getList = lift . DB.getList

-- TODO these instances should no longer be necessary once Groundhog is upgraded.
deriving instance MonadThrow m => MonadThrow (DbPersist conn m)
deriving instance MonadCatch m => MonadCatch (DbPersist conn m)
instance MonadMask m => MonadMask (DbPersist conn m) where
  mask a = DbPersist $ ReaderT $ \e -> mask $ \u -> runReaderT (unDbPersist (a $ q u)) e
    where q u b = DbPersist $ ReaderT $ \e -> u $ runReaderT (unDbPersist b) e
  uninterruptibleMask a = DbPersist $ ReaderT $ \e -> uninterruptibleMask $ \u -> runReaderT (unDbPersist (a $ q u)) e
    where q u b = DbPersist $ ReaderT $ \e -> u $ runReaderT (unDbPersist b) e
