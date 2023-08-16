# android-activity for Haskell

This package lets you embed a haskell application in an android app as an [Android Activity](https://developer.android.com/reference/android/app/Activity).

See java/systems/obsidian/HaskellActivity.java for the core application. We use the [JNI](https://developer.android.com/training/articles/perf-jni) to provide access to android native functions from the Haskell application.
