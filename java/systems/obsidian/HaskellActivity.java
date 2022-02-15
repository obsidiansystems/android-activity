package systems.obsidian;

import android.app.Activity;
import android.annotation.TargetApi;
import android.app.Notification;
import android.app.PendingIntent;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothManager;
import android.bluetooth.BluetoothServerSocket;
import android.bluetooth.BluetoothSocket;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageManager;
import android.Manifest;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.os.ParcelUuid;
import android.util.Log;
import android.webkit.PermissionRequest;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.Thread;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.SynchronousQueue;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import android.webkit.ValueCallback;

public class HaskellActivity extends Activity {
  public native int haskellStartMain(SynchronousQueue<Long> setCallbacks);
  public native void haskellOnCreate(long callbacks);
  public native void haskellOnStart(long callbacks);
  public native void haskellOnResume(long callbacks);
  public native void haskellOnPause(long callbacks);
  public native void haskellOnStop(long callbacks);
  public native void haskellOnDestroy(long callbacks);
  public native void haskellOnRestart(long callbacks);
  public native void haskellOnBackPressed(long callbacks);
  public native void haskellOnNewIntent(long callbacks, String intent, String intentdata);

  public static final int REQUEST_CODE_FILE_PICKER = 51426;

  private BluetoothLib bluetoothLib = new BluetoothLib();

  // Apparently 'long' is the right way to store a C pointer in Java
  // See https://stackoverflow.com/questions/337268/what-is-the-correct-way-to-store-a-native-pointer-inside-a-java-object
  final long callbacks;

  static {
    System.loadLibrary("HaskellActivity");
  }

  public HaskellActivity() throws InterruptedException {
    final SynchronousQueue<Long> setCallbacks = new SynchronousQueue<Long>();
    permissionRequests = new HashMap<Integer, PermissionRequest>();
    new Thread() {
      public void run() {
        final int exitCode = haskellStartMain(setCallbacks);
        Log.d("HaskellActivity", String.format("Haskell main action exited with status %d", exitCode));
        try {
          // Since Haskell's main has exited, it won't call mainStarted.
          // Instead, we unblock the main thread here.
          //TODO: If continueWithCallbacks has already been called, is this safe?
          setCallbacks.put(0L); //TODO: Always call finish() if we hit this
        } catch(InterruptedException e) {
          //TODO: Should we do something with this?
        }
      }
    }.start();
    callbacks = setCallbacks.take();
  }

  // This can be called by the Haskell application to unblock the construction
  // of the HaskellActivity and proceed with the given callbacks.

  // NOTE: This shouldn't be an instance method, because it must be called *before*
  // the constructor returns (it *causes* the constructor to return).
  // 'callbacks' should never be 0
  private static void continueWithCallbacks(SynchronousQueue<Long> setCallbacks, long callbacks) {
    try {
      setCallbacks.put(callbacks);
    } catch(InterruptedException e) {
      Log.d("HaskellActivity", "setting callbacks interrupted");
      //TODO: Should we do something with this?
    }
  }

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    // We can't call finish() in the constructor, as it will have no effect, so
    // we call it here whenever we reach this code without having hit
    // 'continueWithCallbacks'
    if(callbacks == 0) {
      finish();
    } else {
      haskellOnCreate(callbacks); //TODO: Pass savedInstanceState as well
    }
  }

  @Override
  public void onStart() {
    super.onStart();
    if(callbacks != 0) {
      haskellOnStart(callbacks);
    }
  }

  @Override
  public void onResume() {
    super.onResume();
    if(callbacks != 0) {
      haskellOnResume(callbacks);
    }
  }

  @Override
  public void onPause() {
    super.onPause();
    if(callbacks != 0) {
      haskellOnPause(callbacks);
    }
  }

  @Override
  public void onStop() {
    super.onStop();
    if(callbacks != 0) {
      haskellOnStop(callbacks);
    }
  }

  @Override
  public void onDestroy() {
    super.onDestroy();
    if(callbacks != 0) {
      haskellOnDestroy(callbacks);
    }
    //TODO: Should we call hs_exit somehow here?
    android.os.Process.killProcess(android.os.Process.myPid()); //TODO: Properly handle the process surviving between invocations which means that the Haskell RTS needs to not be initialized twice.
  }

  @Override
  public void onRestart() {
    super.onRestart();
    if(callbacks != 0) {
      haskellOnRestart(callbacks);
    }
  }

  @Override
  public void onBackPressed() {
    if(callbacks != 0) {
      haskellOnBackPressed(callbacks);
    }
  }

  @Override
  public void onNewIntent(Intent intent) {
    super.onNewIntent(intent);
    if(callbacks != 0 && intent != null && intent.getData() != null && intent.getAction() != null) {
      haskellOnNewIntent(callbacks, intent.getAction(), intent.getDataString()); //TODO: Use a more canonical way of passing this data - i.e. pass the Intent and let the Haskell side get the data out with JNI
    }
  }

  // function that will show in Android logs that bluetooth has been enabled.
  public void enableBluetooth() {
    bluetoothLib.enableBluetooth(this.getApplicationContext());
  }

  // function that will return a list of Bluetooth device names
  public String scanDevices() {
    return bluetoothLib.scanDevices(this.getApplicationContext());
  }

  public void discoverDevices() {
    //make this device discoverable by a linux receiver app attempting to pair
    Intent discoverableIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_DISCOVERABLE);
    discoverableIntent.putExtra(BluetoothAdapter.EXTRA_DISCOVERABLE_DURATION, 60);
    startActivityForResult(discoverableIntent, 1);
    bluetoothLib.discoverDevices(this.getApplicationContext());

    return;
  }

  public void writeToConnectedDevice(String inputString) {
    bluetoothLib.writeToConnectedDevice(inputString);
    return;
  }

  public String getDiscoveredDevices() {
    return bluetoothLib.getDiscoveredDevices();
  }

  public String establishRFComm(String btDeviceName) {
    return bluetoothLib.establishRFComm(btDeviceName);
  }

  public void cancelBluetoothConnection() {
    bluetoothLib.cancelBluetoothConnection(this.getApplicationContext());
    return;
  }

  // Proper separation of concerns is really a whole lot of work in Java, so
  // we simply handle PermissionRequests directly here - it is just sooo much
  // easier. Java makes it really hard to write good code.
  public void requestWebViewPermissions(final PermissionRequest request) {
    try {
      String[] resources = request.getResources();
      ArrayList<String> sysResourcesToRequestList = new ArrayList<String>();
      for(int i=0; i < resources.length; i++) {
        String manifestRequest = null;
        switch(resources[i]) {
          case PermissionRequest.RESOURCE_AUDIO_CAPTURE:
            manifestRequest =  Manifest.permission.RECORD_AUDIO;
            break;
          case PermissionRequest.RESOURCE_VIDEO_CAPTURE:
            manifestRequest = Manifest.permission.CAMERA;
            break;
        }
        if(manifestRequest != null && checkSelfPermission(manifestRequest) != PackageManager.PERMISSION_GRANTED)
          sysResourcesToRequestList.add(manifestRequest);
      }
      String[] sysResourcesToRequest = sysResourcesToRequestList.toArray(new String[0]);
      if(sysResourcesToRequest.length > 0) {
        permissionRequests.put(nextRequestCode, request);
        requestPermissions(sysResourcesToRequest, nextRequestCode++);
      }
      else {
        runOnUiThread(new Runnable() {
          @TargetApi(Build.VERSION_CODES.LOLLIPOP)
          @Override
          public void run() {
            request.grant(request.getResources());
          }
        });
      }
    } catch (NoSuchMethodError e) { // Compatibility for older Android versions (Android 5 and below)
      runOnUiThread(new Runnable() {
        @TargetApi(Build.VERSION_CODES.LOLLIPOP)
        @Override
        public void run() {
          request.grant(request.getResources());
        }
      });
    }
  }

  @Override
  public void onRequestPermissionsResult(int requestCode, String permissions[], int[] grantResults) {
    final PermissionRequest request = permissionRequests.get(requestCode);
    permissionRequests.remove(requestCode);

    String[] requestedPermissions = request.getResources();
    final HashSet<String> grantedPermissions = new HashSet<String>(Arrays.asList(requestedPermissions));

    // We assume grantResults and permissions have same length ... obviously.
    for(int i = 0; i< permissions.length; i++) {
      if(grantResults[i] == PackageManager.PERMISSION_GRANTED) {
        String permission = null;
        switch(permissions[i]) {
          case Manifest.permission.RECORD_AUDIO:
            Log.d("HaskellActivity", "Granting RESOURCE_AUDIO_CAPTURE!");
            permission = PermissionRequest.RESOURCE_AUDIO_CAPTURE;
            break;
          case Manifest.permission.CAMERA:
            Log.d("HaskellActivity", "Granting RESOURCE_VIDEO_CAPTURE!");
            permission = PermissionRequest.RESOURCE_VIDEO_CAPTURE;
            break;
        }
        if(permission != null && grantResults[i] != PackageManager.PERMISSION_GRANTED)
          grantedPermissions.remove(permission);
      }
    }
    runOnUiThread(new Runnable() {
      @TargetApi(Build.VERSION_CODES.LOLLIPOP)
      @Override
      public void run() {
        if(grantedPermissions.size() > 0) {
          Log.d("HaskellActivity", "Granting permissions!");
          request.grant(grantedPermissions.toArray(new String[0]));
        }
        else {
          request.deny();
        }
      }
    });
  }

  // File uploads don't work out of the box.
  // You have to start an 'Intent' from 'onShowFileChooser' by setting the callback with
  // 'setFileUploadCallback'. The callback will be handled here.
  @Override
  public void onActivityResult(final int requestCode, final int resultCode, final Intent intent) {
    if (requestCode == REQUEST_CODE_FILE_PICKER) {
      if (resultCode == Activity.RESULT_OK) {
        if (intent != null) {
          if (fileUploadCallback != null) {
            Uri[] dataUris = null;

            try {
              if (intent.getDataString() != null) {
                dataUris = new Uri[] { Uri.parse(intent.getDataString()) };
              }
              else {
                if (intent.getClipData() != null) {
                  final int numSelectedFiles = intent.getClipData().getItemCount();
                  dataUris = new Uri[numSelectedFiles];
                  for (int i = 0; i < numSelectedFiles; i++) {
                    dataUris[i] = intent.getClipData().getItemAt(i).getUri();
                  }
                }
              }
            }
            catch (Exception ignored) { }

            fileUploadCallback.onReceiveValue(dataUris);
            fileUploadCallback = null;
          }
        }
      }
      else if (fileUploadCallback != null) {
        fileUploadCallback.onReceiveValue(null);
        fileUploadCallback = null;
      }
    }
  }

  // Set the file upload callback to be used by 'onActivityResult'.
  public void setFileUploadCallback(ValueCallback<Uri[]> cb) {
    if (fileUploadCallback != null) {
      fileUploadCallback.onReceiveValue(null);
    }
    fileUploadCallback = cb;
  }

  private HashMap<Integer, PermissionRequest> permissionRequests;
  private int nextRequestCode = 0;
  private ValueCallback<Uri[]> fileUploadCallback;

}

