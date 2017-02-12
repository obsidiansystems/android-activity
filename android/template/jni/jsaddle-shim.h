#include <jni.h>

#ifndef _included_jsaddle_shim
#define _included_jsaddle_shim

JNIEXPORT void JNICALL Java_systems_obsidian_focus_ProcessJSaddleMessage_processMessageShim (JNIEnv *env, jstring msg);

JNIEXPORT void JNICALL Java_systems_obsidian_focus_JSaddleStart_startHandlerShim (JNIEnv *env);

#endif
