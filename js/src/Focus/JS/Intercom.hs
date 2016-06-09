{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, TemplateHaskell, NoMonomorphismRestriction, EmptyDataDecls, RankNTypes, GADTs, RecursiveDo, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveDataTypeable, GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, UndecidableInstances #-}
module Focus.JS.Intercom
( IntercomUserHash(..)
, IntercomUserSettings(..)
, bootIntercom
, updateIntercom
) where

import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import qualified Data.Text as T

import Focus.JS.App
import Reflex
import Reflex.Dom

import Focus.Intercom.Common

data IntercomUserSettings
  = AnonymousUser
  | IntercomUserSettings
    { intercomUserHash :: IntercomUserHash
    , intercomEmail :: T.Text
    -- Why is this String? Because accountType in Account is a String
    , intercomAccountType :: String
    } deriving (Read, Show)


-- intercom boot
importJS Unsafe "(function(){var o = {}; o['app_id'] = window['intercom_app_id']; console.log('boot', this, o); window['Intercom']('boot', o)})['call'](this)" "intercomBoot" [t| forall x m. MonadJS x m => m () |]

-- intercom update
importJS Unsafe "(function(){var o = {}; o['app_id'] = window['intercom_app_id']; o['email'] = this[0]; o['created_at'] = this[1]; o['name'] = this[2]; o['user_hash'] = this[3]; o['type'] = this[4]; console.log('update', this, o); window['Intercom']('update', o)})['call'](this)" "intercomUpdate" [t| forall x m. MonadJS x m => String -> Int -> String -> String -> String -> m () |] --TODO: Factor out the the 'type' custom attribute into a separate custom attributes argument

-- intercom update anonymous user
importJS Unsafe "(function(){var o = {}; o['app_id'] = window['intercom_app_id']; console.log('updateAnon', this, o); window['Intercom']('updateAnon', o)})['call'](this)" "intercomUpdateAnon" [t| forall x m. MonadJS x m => m () |]

-- intercom shutdown
-- importJS Unsafe "window['Intercom']('shutdown')" "intercomShutdown" [t| forall x m. MonadJS x m => m () |]

updateIntercom :: (MonadFocusWidget app t m, HasJS x (Performable m)) => Behavior t IntercomUserSettings -> m ()
updateIntercom intercomSetingsB = do
  pb <- getPostBuild
  performEvent_ <=< onceE $ ffor pb $ \_ -> do
    intercomSetings <- sample intercomSetingsB
    liftJS $ mkUpdate intercomSetings
  where
    mkUpdate AnonymousUser = intercomUpdateAnon
    mkUpdate intercomUserSettings@IntercomUserSettings{} = intercomUpdate email 0 email iuh accountType
      where
        iuh = T.unpack $ unIntercomUserHash $ intercomUserHash intercomUserSettings
        email = T.unpack $ intercomEmail intercomUserSettings
        accountType = intercomAccountType intercomUserSettings

bootIntercom :: (MonadFocusWidget app t m, HasJS x (Performable m)) => m (Event t ())
bootIntercom = do
  pb <- getPostBuild
  performEvent <=< onceE $ ffor pb $ \_ -> liftJS intercomBoot
