{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeFamilies, UndecidableInstances #-}
module Focus.Backend.TH where

import Language.Haskell.TH
import Control.Monad.State (mapStateT)
import Database.Groundhog
import Control.Monad
import Data.Monoid

deriveNewtypePersistBackend :: (TypeQ -> TypeQ) -> (TypeQ -> TypeQ) -> Name -> Name -> DecsQ
deriveNewtypePersistBackend toT fromT to from =
  liftM (:[]) $ liftM3 InstanceD (cxt [classP ''PersistBackend [fromT m], classP ''Monad [m]]) (appT (conT ''PersistBackend) (toT m)) $ liftM2 (<>) typeInstance functions
  where
    m = varT $ mkName "m"
    typeInstance = liftM (:[]) $ tySynInstD ''PhantomDb $ tySynEqn [toT m] $ appT (conT ''PhantomDb) (fromT m)
    n =: e = valD (varP n) (normalB e) []
    functions = sequence $
      [ 'insert =: [| $(conE to) . insert |]
      , 'insert_ =: [| $(conE to) . insert_ |]
      , 'insertBy =: [| \u v -> $(conE to) $ insertBy u v |]
      , 'insertByAll =: [| $(conE to) . insertByAll |]
      , 'replace =: [| \k v -> $(conE to) $ replace k v |]
      , 'replaceBy =: [| \u v -> $(conE to) $ replaceBy u v |]
      , 'select =: [| $(conE to) . select |]
      , 'selectAll =: [| $(conE to) selectAll |]
      , 'get =: [| $(conE to) . get |]
      , 'getBy =: [| $(conE to) . getBy |]
      , 'update =: [| \us c -> $(conE to) $ update us c |]
      , 'delete =: [| $(conE to) . delete |]
      , 'deleteBy =: [| $(conE to) . deleteBy |]
      , 'deleteAll =: [| $(conE to) . deleteAll |]
      , 'count =: [| $(conE to) . count |]
      , 'countAll =: [| $(conE to) . countAll |]
      , 'project =: [| \p o -> $(conE to) $ project p o |]
      , 'migrate =: [| \v -> mapStateT $(conE to) $ migrate v |]
      , 'executeRaw =: [| \c q p -> $(conE to) $ executeRaw c q p |]
      , 'queryRaw =: [| \c q p f -> $(conE to) $ queryRaw c q p $ \rp -> $(varE from) $ f $ $(conE to) rp |]
      , 'insertList =: [| $(conE to) . insertList |]
      , 'getList =: [| $(conE to) . getList |]
      ]
        {-
      [d|
  --      type PhantomDb $(t $ return $ mkName "m") = PhantomDb $(t' $ return $ mkName "m")
        insert = $(conE to) . insert
        insert_ = $(conE to) . insert_
        insertBy u v = $(conE to) $ insertBy u v
        insertByAll = $(conE to) . insertByAll
        replace k v = $(conE to) $ replace k v
        replaceBy u v = $(conE to) $ replaceBy u v
        select = $(conE to) . select
        selectAll = $(conE to) selectAll
        get = $(conE to) . get
        getBy = $(conE to) . getBy
        update us c = $(conE to) $ update us c
        delete = $(conE to) . delete
        deleteBy = $(conE to) . deleteBy
        deleteAll = $(conE to) . deleteAll
        count = $(conE to) . count
        countAll = $(conE to) . countAll
        project p o = $(conE to) $ project p o
        migrate v = mapStateT $(conE to) $ migrate v
        executeRaw c q p = $(conE to) $ executeRaw c q p
        queryRaw c q p f = $(conE to) $ queryRaw c q p $ \rp -> $(varE from) $ f $ $(conE to) rp
        insertList = $(conE to) . insertList
        getList = $(conE to) . getList
      |]
-}
