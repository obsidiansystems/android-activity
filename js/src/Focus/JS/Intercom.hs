{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, TemplateHaskell, NoMonomorphismRestriction, EmptyDataDecls, RankNTypes, GADTs, RecursiveDo, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveDataTypeable, GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, UndecidableInstances, FunctionalDependencies, AllowAmbiguousTypes, TypeApplications #-}
module Focus.JS.Intercom
       ( IntercomUserHash (..)
       , IntercomVisitor (..)
       , IntercomUserSettings (..)
       , intercom
       , setupIntercom
       , shutdownIntercom
       , displayIntercomChat
       ) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Focus.Intercom.Common
import Focus.JS.App
import Focus.JS.Prerender
import Reflex
import Reflex.Dom

data IntercomVisitor
  = IntercomVisitor_Lead -- ^ Leads are anonymous
  | IntercomVisitor_User IntercomUserSettings -- ^ Users are known to the system and authenticated

data IntercomUserSettings = IntercomUserSettings
  { _intercomUserSettings_hash :: IntercomUserHash
  , _intercomUserSettings_email :: Text
  -- Why is this String? Because accountType in Account is a String
  , _intercomUserSettings_accountType :: String
  } deriving (Read, Show)


-- intercom startup for logged in users
importJS Unsafe "(function(){var o = {}; o['app_id'] = window['intercom_app_id']; o['widget'] = []; o['widget']['activator'] = '#IntercomDefaultWidget'; o['email'] = this[0]; o['created_at'] = this[1]; o['name'] = this[2]; o['user_hash'] = this[3]; o['type'] = this[4]; window['Intercom']('boot', o)})['call'](this)" "intercomStartup" [t| forall x m. MonadJS x m => String -> Int -> String -> String -> String -> m () |] --TODO: Factor out the the 'type' custom attribute into a separate custom attributes argument

-- intercom startup for an anonymous user
importJS Unsafe "(function(){var o = {}; o['app_id'] = window['intercom_app_id']; o['widget'] = {}; o['widget']['activator'] = '#IntercomDefaultWidget'; window['Intercom']('boot', o)})['call'](this)" "intercomStartupAnon" [t| forall x m. MonadJS x m => m () |]

-- intercom shutdown
importJS Unsafe "(function() {window['Intercom']('shutdown');})['call'](this)" "intercomShutdown" [t| forall x m. MonadJS x m => m () |]

-- Show/hide intercom widget
importJS Unsafe "(function() {window['Intercom'](this[0]? 'show' : 'hide');})['call'](this)" "intercomShow" [t| forall x m. MonadJS x m => Bool -> m () |]

-- | Run Intercom and keep it updated with the visitor provided.  When 'Nothing'
-- is provided, intercom will be shut down completely (i.e. not visible on the
-- page).  To specify an anonymous user, use @Just IntercomVisitor_Lead@.
--
-- WARNING: You may only have one of these on the page at time; otherwise they
-- will interfere with each other.
intercom :: (PostBuild t m, PerformEvent t m, Prerender js m) => Dynamic t (Maybe IntercomVisitor) -> m (Event t ())
intercom visitor = prerender (return never) $ do
  pb <- getPostBuild
  let setIntercomSettings = leftmost
        [ updated visitor
        , tag (current visitor) pb
        ]
  performEvent $ ffor setIntercomSettings $ \s -> liftJS $ do
    intercomShutdown
    mapM_ mkUpdate s
  where
    mkUpdate IntercomVisitor_Lead = intercomStartupAnon
    mkUpdate (IntercomVisitor_User intercomUserSettings) = intercomStartup email 0 email iuh accountType
      where
        iuh = T.unpack $ unIntercomUserHash $ _intercomUserSettings_hash intercomUserSettings
        email = T.unpack $ _intercomUserSettings_email intercomUserSettings
        accountType = _intercomUserSettings_accountType intercomUserSettings

-- TODO: Make this automatic when the 'intercom' widget disappears
{-# DEPRECATED shutdownIntercom "Use 'intercom (constDyn Nothing)' instead" #-}
shutdownIntercom :: (PostBuild t m, PerformEvent t m, Prerender js m) => m (Event t ())
shutdownIntercom = intercom $ constDyn Nothing

{-# DEPRECATED setupIntercom "Use 'void . intercom . fmap Just =<< holdDyn v0 newV' instead of 'setupIntercom v0 newV'" #-}
setupIntercom :: (MonadFocusWidget app t m, Prerender js m) => IntercomVisitor -> Event t IntercomVisitor -> m ()
setupIntercom v0 v' = void . intercom . fmap Just =<< holdDyn v0 v'

-- | Show or hide intercom chat widget
displayIntercomChat :: (PerformEvent t m, Prerender js m) => Event t Bool -> m (Event t ())
displayIntercomChat eTrigger = prerender (return never) $ performEvent $ ffor eTrigger $ liftJS . intercomShow
