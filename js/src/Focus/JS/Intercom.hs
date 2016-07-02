{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, TemplateHaskell, NoMonomorphismRestriction, EmptyDataDecls, RankNTypes, GADTs, RecursiveDo, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveDataTypeable, GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, UndecidableInstances #-}
module Focus.JS.Intercom
( IntercomUserHash(..)
, IntercomUserSettings(..)
, setupIntercom
, shutdownIntercom
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


-- intercom startup for logged in users
importJS Unsafe "(function(){var o = {}; o['app_id'] = window['intercom_app_id']; o['widget'] = {activator: '#IntercomDefaultWidget'}; o['email'] = this[0]; o['created_at'] = this[1]; o['name'] = this[2]; o['user_hash'] = this[3]; o['type'] = this[4]; console.log('startup', this, o); window['Intercom']('boot', o)})['call'](this)" "intercomStartup" [t| forall x m. MonadJS x m => String -> Int -> String -> String -> String -> m () |] --TODO: Factor out the the 'type' custom attribute into a separate custom attributes argument

-- intercom startup for an anonymous user
importJS Unsafe "(function(){var o = {}; o['app_id'] = window['intercom_app_id']; o['widget'] = {activator: '#IntercomDefaultWidget'}; console.log('startup anon', this, o); window['Intercom']('boot', o)})['call'](this)" "intercomStartupAnon" [t| forall x m. MonadJS x m => m () |]

-- intercom shutdown
importJS Unsafe "(function() {console.log('shutdown'); window['Intercom']('shutdown');})['call'](this)" "intercomShutdown" [t| forall x m. MonadJS x m => m () |]

setupIntercom :: (MonadFocusWidget app t m, HasJS x (Performable m)) => IntercomUserSettings -> Event t IntercomUserSettings -> m ()
setupIntercom currentIntercomSettings intercomSettingsChangeEvt = do
  pb <- getPostBuild
  performEvent_ $ fmap (\u -> liftJS intercomShutdown >> liftJS (mkUpdate u)) (mergeWith (flip const) [fmap (const currentIntercomSettings) pb, intercomSettingsChangeEvt])
  where
    mkUpdate AnonymousUser = intercomStartupAnon
    mkUpdate intercomUserSettings@IntercomUserSettings{} = intercomStartup email 0 email iuh accountType
      where
        iuh = T.unpack $ unIntercomUserHash $ intercomUserHash intercomUserSettings
        email = T.unpack $ intercomEmail intercomUserSettings
        accountType = intercomAccountType intercomUserSettings

shutdownIntercom :: (MonadFocusWidget app t m, HasJS x (Performable m)) => m (Event t ())
shutdownIntercom = do
  pb <- getPostBuild
  performEvent <=< onceE $ ffor pb $ \_ -> liftJS intercomShutdown
