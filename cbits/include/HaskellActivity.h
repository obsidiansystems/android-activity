#ifndef HASKELLACTIVITY_H_INCLUDED
#define HASKELLACTIVITY_H_INCLUDED

#include <jni.h>

typedef struct ActivityCallbacks {
  void (*onCreate) (); //TODO: Support savedInstanceState
  void (*onStart) ();
  void (*onResume) ();
  void (*onPause) ();
  void (*onStop) ();
  void (*onDestroy) ();
  void (*onRestart) ();
  void (*onBackPressed) ();
  void (*onNewIntent) (const char *, const char *); //TODO: Pass the whole argument and use JNI
  void (*firebaseInstanceIdService_sendRegistrationToServer) (char *);
} ActivityCallbacks;

extern JavaVM* HaskellActivity_jvm;

extern jobject HaskellActivity_get();

#endif
