{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Android.HaskellActivity
  ( ActivityCallbacks (..)
  , HaskellActivity (..)
  , getHaskellActivity
  , getFilesDir
  , getCacheDir
  , continueWithCallbacks
  , traceActivityCallbacks
  , JNINativeInterface_
  , JNIEnv
  , JNI (..)
  , runJNI
  , Jobject (..)
  , Jclass (..)
  , JmethodID (..)
  , Jvalue (..)
  , Jstring (..)
  , ToJobject (..)
  , ToJvalue (..)
  , findClass
  , getObjectClass
  , getMethodID
  , callObjectMethod
  , callVoidMethod
  , callIntMethod
  , newGlobalRef
  , deleteGlobalRef
  , newStringUTF
  , exceptionOccurred
  , exceptionDescribe
  , exceptionClear
  ) where

import Control.Exception
import Control.Monad
import Data.Default
import Debug.Trace
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Control.Monad.Reader
import Control.Concurrent
import Foreign.Marshal.Array
import Data.Int

#include "HaskellActivity.h"

newtype HaskellActivity = HaskellActivity { unHaskellActivity :: Ptr HaskellActivity }

data JNINativeInterface_

type JNIEnv = Ptr JNINativeInterface_

foreign import ccall safe "getJNIEnv" getJNIEnv :: IO (Ptr JNIEnv)

newtype JNI a = JNI { unJNI :: ReaderT (Ptr JNIEnv) IO a } deriving (Functor, Applicative, Monad, MonadIO)

--TODO: Manage references?
runJNI :: JNI a -> IO a
runJNI (JNI a) = runInBoundThread $ do -- JNIEnvs are bound to a specific OS thread
  jniEnv <- getJNIEnv
  runReaderT a jniEnv

newtype Jobject = Jobject { unJobject :: Ptr () } deriving (Eq, Show, Storable)

newtype Jclass = Jclass { unJclass :: Ptr () } deriving (Eq, Show, Storable)

newtype JmethodID = JmethodID { unJmethodID :: Ptr () } deriving (Eq, Show, Storable)

newtype Jvalue = Jvalue { unJvalue :: Ptr () } deriving (Eq, Show, Storable)

newtype Jstring = Jstring { unJstring :: Ptr () } deriving (Eq, Show, Storable)

class ToJobject a where
  toJobject :: a -> Jobject

instance ToJobject Jstring where
  toJobject (Jstring a) = Jobject a

class ToJvalue a where
  toJvalue :: a -> Jvalue

instance ToJvalue Jstring where
  toJvalue (Jstring a) = Jvalue a

instance ToJvalue Jobject where
  toJvalue (Jobject a) = Jvalue a

foreign import ccall "dynamic" funFindClass :: FunPtr (Ptr JNIEnv -> CString -> IO Jclass) -> Ptr JNIEnv -> CString -> IO Jclass

findClass :: String -> JNI Jclass
findClass name = JNI $ ReaderT $ \env -> do
  f <- #{peek struct JNINativeInterface_, FindClass} =<< peek env
  --TODO: Technically we need to be using Java's "modified UTF-8" here
  withCString name $ \nameRaw ->
    funFindClass f env nameRaw

foreign import ccall "dynamic" funGetObjectClass :: FunPtr (Ptr JNIEnv -> Jobject -> IO Jclass) -> Ptr JNIEnv -> Jobject -> IO Jclass

getObjectClass :: Jobject -> JNI Jclass
getObjectClass o = JNI $ ReaderT $ \env -> do
  f <- #{peek struct JNINativeInterface_, GetObjectClass} =<< peek env
  funGetObjectClass f env o

foreign import ccall "dynamic" funGetMethodID :: FunPtr (Ptr JNIEnv -> Jclass -> CString -> CString -> IO JmethodID) -> Ptr JNIEnv -> Jclass -> CString -> CString -> IO JmethodID

getMethodID :: Jclass -> String -> String -> JNI JmethodID
getMethodID c name sig = JNI $ ReaderT $ \env -> do
  f <- #{peek struct JNINativeInterface_, GetMethodID} =<< peek env
  --TODO: Technically we need to be using Java's "modified UTF-8" here
  withCString name $ \nameC ->
    withCString sig $ \sigC ->
      funGetMethodID f env c nameC sigC

foreign import ccall "dynamic" funCallObjectMethodA :: FunPtr (Ptr JNIEnv -> Jobject -> JmethodID -> Ptr Jvalue -> IO Jobject) -> Ptr JNIEnv -> Jobject -> JmethodID -> Ptr Jvalue -> IO Jobject

callObjectMethod :: Jobject -> JmethodID -> [Jvalue] -> JNI Jobject
callObjectMethod o m args = JNI $ ReaderT $ \env -> do
  f <- #{peek struct JNINativeInterface_, CallObjectMethodA} =<< peek env
  withArray args $ \argsRaw ->
    funCallObjectMethodA f env o m argsRaw

foreign import ccall "dynamic" funCallVoidMethodA :: FunPtr (Ptr JNIEnv -> Jobject -> JmethodID -> Ptr Jvalue -> IO ()) -> Ptr JNIEnv -> Jobject -> JmethodID -> Ptr Jvalue -> IO ()

callVoidMethod :: Jobject -> JmethodID -> [Jvalue] -> JNI ()
callVoidMethod o m args = JNI $ ReaderT $ \env -> do
  f <- #{peek struct JNINativeInterface_, CallVoidMethodA} =<< peek env
  withArray args $ \argsRaw ->
    funCallVoidMethodA f env o m argsRaw

foreign import ccall "dynamic" funCallIntMethodA :: FunPtr (Ptr JNIEnv -> Jobject -> JmethodID -> Ptr Jvalue -> IO Int32) -> Ptr JNIEnv -> Jobject -> JmethodID -> Ptr Jvalue -> IO Int32

callIntMethod :: Jobject -> JmethodID -> [Jvalue] -> JNI Int32
callIntMethod o m args = JNI $ ReaderT $ \env -> do
  f <- #{peek struct JNINativeInterface_, CallIntMethodA} =<< peek env
  withArray args $ \argsRaw ->
    funCallIntMethodA f env o m argsRaw

foreign import ccall "dynamic" funNewGlobalRef :: FunPtr (Ptr JNIEnv -> Jobject -> IO Jobject) -> Ptr JNIEnv -> Jobject -> IO Jobject

newGlobalRef :: Jobject -> JNI Jobject
newGlobalRef o = JNI $ ReaderT $ \env -> do
  f <- #{peek struct JNINativeInterface_, NewGlobalRef} =<< peek env
  --TODO: Deal with failure
  funNewGlobalRef f env o

foreign import ccall "dynamic" funDeleteGlobalRef :: FunPtr (Ptr JNIEnv -> Jobject -> IO ()) -> Ptr JNIEnv -> Jobject -> IO ()

deleteGlobalRef :: Jobject -> JNI ()
deleteGlobalRef o = JNI $ ReaderT $ \env -> do
  f <- #{peek struct JNINativeInterface_, DeleteGlobalRef} =<< peek env
  funDeleteGlobalRef f env o

foreign import ccall "dynamic" funNewStringUTF :: FunPtr (Ptr JNIEnv -> CString -> IO Jstring) -> Ptr JNIEnv -> CString -> IO Jstring

newStringUTF :: String -> JNI Jstring
newStringUTF s = JNI $ ReaderT $ \env -> do
  f <- #{peek struct JNINativeInterface_, NewStringUTF} =<< peek env
  --TODO: Technically we need to be using Java's "modified UTF-8" here
  withCString s $ \sc ->
    funNewStringUTF f env sc

foreign import ccall "dynamic" funExceptionOccurred :: FunPtr (Ptr JNIEnv -> IO Bool) -> Ptr JNIEnv -> IO Bool

exceptionOccurred :: JNI Bool
exceptionOccurred = JNI $ ReaderT $ \env -> do
  f <- #{peek struct JNINativeInterface_, ExceptionOccurred} =<< peek env
  funExceptionOccurred f env

foreign import ccall "dynamic" funExceptionDescribe :: FunPtr (Ptr JNIEnv -> IO ()) -> Ptr JNIEnv -> IO ()

exceptionDescribe :: JNI ()
exceptionDescribe = JNI $ ReaderT $ \env -> do
  f <- #{peek struct JNINativeInterface_, ExceptionDescribe} =<< peek env
  funExceptionDescribe f env

foreign import ccall "dynamic" funExceptionClear :: FunPtr (Ptr JNIEnv -> IO ()) -> Ptr JNIEnv -> IO ()

exceptionClear :: JNI ()
exceptionClear = JNI $ ReaderT $ \env -> do
  f <- #{peek struct JNINativeInterface_, ExceptionClear} =<< peek env
  funExceptionClear f env

foreign import ccall unsafe "HaskellActivity_get" getHaskellActivity :: IO HaskellActivity

foreign import ccall unsafe "HaskellActivity_getFilesDir" getFilesDirCString
  :: HaskellActivity
  -> IO CString

foreign import ccall unsafe "HaskellActivity_getCacheDir" getCacheDirCString
  :: HaskellActivity
  -> IO CString

-- | Copy a C string into Haskell returning 'Nothing' if it is NULL.
peekMaybeCString :: CString -> IO (Maybe String)
peekMaybeCString str =
  if str == nullPtr
  then return Nothing
  else Just <$> peekCString str

-- | Get the "internal" storage directory for the app. This is where data local
-- to the app should be stored. Note that 'Nothing' is returned if the activity
-- is not fully initalized. In practice this means you probably need to call
-- this inside the main widget.
getFilesDir :: HaskellActivity -> IO (Maybe FilePath)
getFilesDir = getFilesDirCString >=> peekMaybeCString

-- | Get the cache storage directory for the app. Android may delete this data
-- at any time. Note that 'Nothing' is returned if the activity is not fully
-- initalized. In practice this means you probably need to call this inside the
-- main widget.
getCacheDir :: HaskellActivity -> IO (Maybe FilePath)
getCacheDir = getCacheDirCString >=> peekMaybeCString

-- | Allow the HaskellActivity to proceed.  The given callbacks will be invoked
-- at the appropriate times in the Android Activity lifecycle.
-- WARNING: This should only be invoked once per application execution.
continueWithCallbacks :: ActivityCallbacks -> IO ()
continueWithCallbacks ac = do
  continueWithCallbacks_ =<< new =<< activityCallbacksToPtrs ac

foreign import ccall safe "HaskellActivity_continueWithCallbacks" continueWithCallbacks_ :: Ptr ActivityCallbacksPtrs -> IO ()

data ActivityCallbacks = ActivityCallbacks
  { _activityCallbacks_onCreate :: () -> IO () -- The () input here will eventually become a representation of the Bundle that Android passes in; this placeholder is to make the change easier
  , _activityCallbacks_onStart :: IO ()
  , _activityCallbacks_onResume :: IO ()
  , _activityCallbacks_onPause :: IO ()
  , _activityCallbacks_onStop :: IO ()
  , _activityCallbacks_onDestroy :: IO ()
  , _activityCallbacks_onRestart :: IO ()
  , _activityCallbacks_onBackPressed :: IO ()
  , _activityCallbacks_onNewIntent :: String -> String -> IO ()
  , _activityCallbacks_firebaseInstanceIdServiceSendRegistrationToServer :: String -> IO ()
  }

instance Default ActivityCallbacks where
  def = ActivityCallbacks
    { _activityCallbacks_onCreate = \_ -> return ()
    , _activityCallbacks_onStart = return ()
    , _activityCallbacks_onResume = return ()
    , _activityCallbacks_onPause = return ()
    , _activityCallbacks_onStop = return ()
    , _activityCallbacks_onDestroy = return ()
    , _activityCallbacks_onRestart = return ()
    , _activityCallbacks_onBackPressed = return ()
    , _activityCallbacks_onNewIntent = \_ _ -> return ()
    , _activityCallbacks_firebaseInstanceIdServiceSendRegistrationToServer = \_ -> return ()
    }

traceBracket :: String -> IO a -> IO a
traceBracket s = bracket (traceIO $ s <> " entered") (\_ -> traceIO $ s <> " exited") . const

traceActivityCallbacks :: ActivityCallbacks -> ActivityCallbacks
traceActivityCallbacks ac = ActivityCallbacks
  { _activityCallbacks_onCreate = \x -> traceBracket "onCreate" $ _activityCallbacks_onCreate ac x
  , _activityCallbacks_onStart = traceBracket "onStart" $ _activityCallbacks_onStart ac
  , _activityCallbacks_onResume = traceBracket "onResume" $ _activityCallbacks_onResume ac
  , _activityCallbacks_onPause = traceBracket "onPause" $ _activityCallbacks_onPause ac
  , _activityCallbacks_onStop = traceBracket "onStop" $ _activityCallbacks_onStop ac
  , _activityCallbacks_onDestroy = traceBracket "onDestroy" $ _activityCallbacks_onDestroy ac
  , _activityCallbacks_onRestart = traceBracket "onRestart" $ _activityCallbacks_onRestart ac
  , _activityCallbacks_onNewIntent = \x y -> traceBracket "onNewIntent" $ _activityCallbacks_onNewIntent ac x y
  , _activityCallbacks_onBackPressed = traceBracket "onBackPressed" $ _activityCallbacks_onBackPressed ac
  , _activityCallbacks_firebaseInstanceIdServiceSendRegistrationToServer = \x ->
      traceBracket "firebaseInstanceIdServiceSendRegistrationToServer" $ _activityCallbacks_firebaseInstanceIdServiceSendRegistrationToServer ac x
  }

foreign import ccall "wrapper" wrapIO :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "wrapper" wrapCStringIO :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))
foreign import ccall "wrapper" wrapCStringCStringIO :: (CString -> CString -> IO ()) -> IO (FunPtr (CString -> CString -> IO ()))

activityCallbacksToPtrs :: ActivityCallbacks -> IO ActivityCallbacksPtrs
activityCallbacksToPtrs ac = ActivityCallbacksPtrs
  <$> wrapIO (_activityCallbacks_onCreate ac ())
  <*> wrapIO (_activityCallbacks_onStart ac)
  <*> wrapIO (_activityCallbacks_onResume ac)
  <*> wrapIO (_activityCallbacks_onPause ac)
  <*> wrapIO (_activityCallbacks_onStop ac)
  <*> wrapIO (_activityCallbacks_onDestroy ac)
  <*> wrapIO (_activityCallbacks_onRestart ac)
  <*> wrapIO (_activityCallbacks_onBackPressed ac)
  <*> wrapCStringCStringIO (\a b -> do
        a' <- peekCString a
        b' <- peekCString b
        _activityCallbacks_onNewIntent ac a' b'
      )
  <*> wrapCStringIO (\token -> do
        token' <- peekCString token
        _activityCallbacks_firebaseInstanceIdServiceSendRegistrationToServer ac token'
      )

data ActivityCallbacksPtrs = ActivityCallbacksPtrs
  { _activityCallbacksPtrs_onCreate :: FunPtr (IO ())
  , _activityCallbacksPtrs_onStart :: FunPtr (IO ())
  , _activityCallbacksPtrs_onResume :: FunPtr (IO ())
  , _activityCallbacksPtrs_onPause :: FunPtr (IO ())
  , _activityCallbacksPtrs_onStop :: FunPtr (IO ())
  , _activityCallbacksPtrs_onDestroy :: FunPtr (IO ())
  , _activityCallbacksPtrs_onRestart :: FunPtr (IO ())
  , _activityCallbacksPtrs_onBackPressed :: FunPtr (IO ())
  , _activityCallbacksPtrs_onNewIntent :: FunPtr (CString -> CString -> IO ())
  , _activityCallbacksPtrs_firebaseInstanceIdService_sendRegistrationToServer :: FunPtr (CString -> IO ())
  }

instance Storable ActivityCallbacksPtrs where
  sizeOf _ = #{size ActivityCallbacks}
  alignment _ = #{alignment ActivityCallbacks}
  poke p ac = do
    #{poke ActivityCallbacks, onCreate} p $ _activityCallbacksPtrs_onCreate ac
    #{poke ActivityCallbacks, onStart} p $ _activityCallbacksPtrs_onStart ac
    #{poke ActivityCallbacks, onResume} p $ _activityCallbacksPtrs_onResume ac
    #{poke ActivityCallbacks, onPause} p $ _activityCallbacksPtrs_onPause ac
    #{poke ActivityCallbacks, onStop} p $ _activityCallbacksPtrs_onStop ac
    #{poke ActivityCallbacks, onDestroy} p $ _activityCallbacksPtrs_onDestroy ac
    #{poke ActivityCallbacks, onRestart} p $ _activityCallbacksPtrs_onRestart ac
    #{poke ActivityCallbacks, onBackPressed} p $ _activityCallbacksPtrs_onBackPressed ac
    #{poke ActivityCallbacks, onNewIntent} p $ _activityCallbacksPtrs_onNewIntent ac
    #{poke ActivityCallbacks, firebaseInstanceIdService_sendRegistrationToServer} p $ _activityCallbacksPtrs_firebaseInstanceIdService_sendRegistrationToServer ac
  peek p = ActivityCallbacksPtrs
    <$> #{peek ActivityCallbacks, onCreate} p
    <*> #{peek ActivityCallbacks, onStart} p
    <*> #{peek ActivityCallbacks, onResume} p
    <*> #{peek ActivityCallbacks, onPause} p
    <*> #{peek ActivityCallbacks, onStop} p
    <*> #{peek ActivityCallbacks, onDestroy} p
    <*> #{peek ActivityCallbacks, onRestart} p
    <*> #{peek ActivityCallbacks, onBackPressed} p
    <*> #{peek ActivityCallbacks, onNewIntent} p
    <*> #{peek ActivityCallbacks, firebaseInstanceIdService_sendRegistrationToServer} p
