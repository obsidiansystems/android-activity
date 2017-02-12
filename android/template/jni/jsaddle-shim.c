#include <string.h>
#include <jni.h>
#include "jsaddle-shim.h"

/*
foreign import ccall addJSaddleHandler :: WKWebView -> StablePtr (IO ()) -> StablePtr (Results -> IO ()) -> IO ()
foreign import ccall loadHTMLString :: WKWebView -> CString -> IO ()
foreign import ccall evaluateJavaScript :: WKWebView -> CString -> IO ()

foreign export ccall jsaddleStart :: StablePtr (IO ()) -> IO ()
foreign export ccall jsaddleResult :: StablePtr (Results -> IO ()) -> CString -> IO ()
foreign export ccall withWebView :: WKWebView -> StablePtr (WKWebView -> IO ()) -> IO ()
*/

JNIEXPORT void JNICALL Java_systems_obsidian_aspen_ProcessJSaddleMessage_processMessageShim (JNIEnv *env, jstring msg) {
  return;
}

JNIEXPORT void JNICALL Java_systems_obsidian_focus_JSaddleStart_startHandlerShim (JNIEnv *env) {
  return;
}
