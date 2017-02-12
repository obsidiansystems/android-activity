LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE := jsaddle-shim
LOCAL_SRC_FILES := jsaddle-shim.c

include $(BUILD_SHARED_LIBRARY)
