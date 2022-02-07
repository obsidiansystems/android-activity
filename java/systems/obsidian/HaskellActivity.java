package systems.obsidian;

import android.app.Activity;
import android.annotation.TargetApi;
import android.app.Activity;
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
    unregisterReceiver(receiver);
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
    BluetoothAdapter bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
    if (bluetoothAdapter == null) {
      Log.v("HaskellActivity", "bluetoothAdapter is null");
    } else {
      Log.v("HaskellActivity", "bluetoothAdapter obtained");
    }

    //Enable bluetooth if it isn't already enabled
    if (!bluetoothAdapter.isEnabled()) {
      Log.v("HaskellActivity", "Enabling bluetooth...");
      Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
      this.getApplicationContext().startActivity(enableBtIntent);
      Log.v("HaskellActivity", "...bluetooth has been enabled.");
    } else {
      Log.v("HaskellActivity", "Bluetooth enabled");
    }
  }

  // function that will return a list of Bluetooth device names
  public String scanDevices() {
    BluetoothAdapter bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
    if (bluetoothAdapter == null) {
      Log.v("HaskellActivity", "bluetoothAdapter is null");
    } else {
      Log.v("HaskellActivity", "bluetoothAdapter obtained");
    }

    //Enable bluetooth if it isn't already enabled
    if (!bluetoothAdapter.isEnabled()) {
      Log.v("HaskellActivity", "Enabling bluetooth...");
      Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
      this.getApplicationContext().startActivity(enableBtIntent);
      Log.v("HaskellActivity", "...bluetooth has been enabled.");
    } else {
      Log.v("HaskellActivity", "Bluetooth enabled");
    }

    Set<BluetoothDevice> pairedDevices = bluetoothAdapter.getBondedDevices();
    ArrayList<String> deviceNames = new ArrayList<String>();
    for (BluetoothDevice bt : pairedDevices) {
      deviceNames.add(bt.getName() + "|" + bt.getAddress());
      connectionReadyDevices.add(bt);
    }

    String[] deviceNameArray = deviceNames.toArray(new String[deviceNames.size()]);

    Log.v("HaskellActivity", "returning deviceNameArray...");
    return String.join(",", deviceNameArray);
  }

  public void discoverDevices() {
    BluetoothAdapter bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
    if (bluetoothAdapter == null) {
      Log.v("HaskellActivity", "bluetoothAdapter is null");
    } else {
      Log.v("HaskellActivity", "bluetoothAdapter obtained");
    }

    //Enable bluetooth if it isn't already enabled
    if (!bluetoothAdapter.isEnabled()) {
      Log.v("HaskellActivity", "Enabling bluetooth...");
      Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
      this.getApplicationContext().startActivity(enableBtIntent);
      Log.v("HaskellActivity", "...bluetooth has been enabled.");
    } else {
      Log.v("HaskellActivity", "Bluetooth enabled");
    }

    //make this device discoverable by a linux receiver app attempting to pair
    Intent discoverableIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_DISCOVERABLE);
    discoverableIntent.putExtra(BluetoothAdapter.EXTRA_DISCOVERABLE_DURATION, 60);
    startActivityForResult(discoverableIntent, 1);

    IntentFilter filter = new IntentFilter(BluetoothDevice.ACTION_FOUND);
    registerReceiver(receiver, filter);
    Log.v("HaskellActivity", "Receiver registered");
    // Pre-check to make sure device is not already discovering
    if (bluetoothAdapter.isDiscovering()) {
      bluetoothAdapter.cancelDiscovery();
    }
    bluetoothAdapter.startDiscovery();
    // Give discovery a moment to start up
    try {
      Thread.sleep(2000);
    } catch(InterruptedException e) {
      Log.v("HaskellActivity", "Thread.sleep exception thrown");
    }
    if (bluetoothAdapter.isDiscovering()) {
      Log.v("HaskellActivity", "Discovery started...");
      // Give 10 seconds for discovery to discover devices
      try {
        Thread.sleep(10000);
      } catch(InterruptedException e) {
        Log.v("HaskellActivity", "Thread.sleep exception thrown");
      }
      Log.v("HaskellActivity", "Cancelling discovery...");
      boolean discoveryDisabled = bluetoothAdapter.cancelDiscovery();
      if (discoveryDisabled) {
        Log.v("HaskellActivity", "Discovery cancelled");
      } else {
        Log.v("HaskellActivity", "Discovery cancellation unsuccessful");
      }
    } else {
      /* ContactsContract.CommonDataKinds.Note: If bluetooth adapter never started,
      ** it's likely that location is not enabled by the user.
      ** Check within Settings -> App -> "this app name" ->
      ** permissions on the android device */
      Log.v("HaskellActivity", "Discovery was not started.");
    }
  }

  public ArrayList<String> discoveredDevices = new ArrayList<String>();
  public ArrayList<BluetoothDevice> connectionReadyDevices = new ArrayList<BluetoothDevice>();

  // Outputstream in order to write to connected devices. Has been set at this level in order to be accessed
  // by Reflex via FFI imports to pass strings from input dom element to connected bluetooth device
  private OutputStream mmOutStream;
  private byte[] mmBuffer = new byte[1024];

  public void writeToConnectedDevice(String inputString) {
    byte[] bytes = inputString.getBytes();
    try {
      mmOutStream.write(bytes);
    } catch (IOException e){
      Log.e("HaskellActivity", ("Error while sending data:" + e));
      Bundle bundle = new Bundle();
      bundle.putString("toast", "Data could not be sent to other device");
    }
  }

  private final BroadcastReceiver receiver = new BroadcastReceiver() {
    public void onReceive(Context context, Intent intent) {
      String action = intent.getAction();
      if (BluetoothDevice.ACTION_FOUND.equals(action)) {
        BluetoothDevice device = intent.getParcelableExtra(BluetoothDevice.EXTRA_DEVICE);
        String deviceName = device.getName();
        String deviceHardwareAddress = device.getAddress();
        // In average Android Studio apps, the view would be updated within this function
        // Since we are using Reflex and Webview, this information will be pushed to a
        // broader scoped variable, and retrieved on a reflex frontend using a getter function
        // getDiscoveredDevices()
        Log.v("HaskellActivity", ("Discovered the following device: " + deviceName));

        discoveredDevices.add(deviceName + "|" + deviceHardwareAddress);
        connectionReadyDevices.add(device);
      }
    }
  };


  public String getDiscoveredDevices() {
    String[] deviceNameArray = discoveredDevices.toArray(new String[discoveredDevices.size()]);
    // discoveredDevices.clear();
    return String.join(",", deviceNameArray);
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

  private boolean isAcceptThreadInProgress = false;

  public void establishRFComm(String btDeviceName) {
    if (! isAcceptThreadInProgress) {
      isAcceptThreadInProgress = true;
      AcceptThread acceptThread = new AcceptThread(btDeviceName);
      Log.v("HaskellActivity", ("Establishing bluetooth communication with " + btDeviceName + "..."));
      try {
        Thread.sleep(1000);
      } catch(InterruptedException e) {
        Log.v("HaskellActivity", "Thread.sleep exception thrown");
      }
      acceptThread.start();
    } else {
      Log.v("HaskellActivity", ("AcceptThread already in progress..."));
    }
  }

  class AcceptThread extends Thread {
    private final BluetoothServerSocket mmServerSocket;

    public BluetoothDevice getBluetoothDeviceInfo(String dvAddr, ArrayList<BluetoothDevice> dvs) {
      if (! dvs.isEmpty()) {
        Log.v("HaskellActivity", ("getBluetoothDeviceInfo: LOOKING FOR " + dvAddr));
        for (BluetoothDevice dv : dvs) {
          Log.v("HaskellActivity", ("LOOKING AT " + dv.getName() + " with mac address " + dv.getAddress()));
          if (dv.getAddress().equals(dvAddr)) {
            Log.v("HaskellActivity", "bluetoothDevice hardware address found");
            return dv;
          }
        }
        Log.v("HaskellActivity", "bluetoothDevice hardware address not found");
        return null;
      }
      Log.v("HaskellActivity", "empty list was passed to getBluetoothDeviceInfo");
      return null;
    }

    public AcceptThread(String deviceAddr) {
      BluetoothServerSocket tmp = null;

      // TODO: we should pass this adapter through the constructor
      BluetoothAdapter bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
      if (bluetoothAdapter == null) {
        Log.v("HaskellActivity", "bluetoothAdapter is null");
      } else {
        Log.v("HaskellActivity", "bluetoothAdapter obtained");
      }

      Log.v("HaskellActivity", "Getting Device info....");
      // Retreive bluetooth device information for use when establishing RFComm
      BluetoothDevice btDevice = getBluetoothDeviceInfo(deviceAddr, connectionReadyDevices);
      Log.v("HaskellActivity", "Getting UUID info....");
      ParcelUuid[] uuids = btDevice.getUuids();
      if (!(uuids.length <= 0)) {
        Log.v("HaskellActivity", "UUID info obtained.");

        try {
          Log.v("HaskellActivity", ("listenUsingRfcommWithServiceRecord initiating with UUID: " + uuids[0].getUuid()));
          tmp = bluetoothAdapter.listenUsingRfcommWithServiceRecord(btDevice.getName(), uuids[0].getUuid());
        } catch (IOException e) {
          Log.e("HaskellActivity", ("Socket listen failed" + e));
          isAcceptThreadInProgress = false;
        }
        Log.v("HaskellActivity", "setting mmServerSocket...");
        mmServerSocket = tmp;
      } else {
        Log.e("HaskellActivity", "Bluetooth Device object not found.");
        mmServerSocket = null;
      }
    }

    public void run () {
      BluetoothSocket socket = null;

      while (true) {
        try {
          Log.v("HaskellActivity", "accepting server socket...");
          socket = mmServerSocket.accept();
          Log.v("HaskellActivity", "server socket accepted.");
        } catch (IOException e) {
          Log.e("HaskellActivity", ("Socket accept failed: " + e));
          isAcceptThreadInProgress = false;
          break;
        }

        if (socket != null) {
          Log.v("HaskellActivity", "connecting thread...");
          ConnectedThread connectedThread = new ConnectedThread(socket);
          Log.v("HaskellActivity", "running thread...");
          try {
            Thread.sleep(1000);
          } catch(InterruptedException e) {
            Log.v("HaskellActivity", "Thread.sleep exception thrown");
          }
          connectedThread.start();
          try {
            mmServerSocket.close();
          } catch (IOException e) {
            Log.e("HaskellActivity", ("Failed to close server socket: " + e));
            isAcceptThreadInProgress = false;
          }
          break;
        }
      }
    }

    public void cancel() {
      try {
        mmServerSocket.close();
      } catch (IOException e) {
        Log.e("HaskellActivity", ("Failed to close connection socket: " + e));
        isAcceptThreadInProgress = false;
      }
    }
  }

  private Handler handler; //fetches Bluetooth service info

  private interface MessageConstants {
    public static final int MESSAGE_READ = 0;
    public static final int MESSAGE_WRITE = 1;
    public static final int MESSAGE_TOAST = 2;
  }

  class ConnectedThread extends Thread {
    private final BluetoothSocket mmSocket;
    private final InputStream mmInStream;
    // private final OutputStream mmOutStream;
    private byte[] mmBuffer;

    public ConnectedThread(BluetoothSocket socket) {
      mmSocket = socket;
      InputStream tmpIn = null;
      OutputStream tmpOut = null;

      try {
        Log.v("HaskellActivity", "getting InputStream...");
        tmpIn = socket.getInputStream();
      } catch (IOException e) {
        Log.e("HaskellActivity", ("Error while creating input stream: " + e));
        isAcceptThreadInProgress = false;
      }
      try{
        Log.v("HaskellActivity", "getting OutputStream...");
        tmpOut = socket.getOutputStream();
      } catch (IOException e) {
        Log.e("HaskellActivity", ("Error while creating output stream: " + e));
        isAcceptThreadInProgress = false;
      }

      Log.v("HaskellActivity", "steams secured, setting streams");
      mmInStream = tmpIn;
      mmOutStream = tmpOut;
    }

    public void run() {
      int numBytes;

      Log.v("HaskellActivity", "ready to read incoming bytes...");

      while (true) {
        try {
          if (mmBuffer!= null) {
            Log.v("HaskellActivity", "reading bytes...");
            numBytes = mmInStream.read(mmBuffer);
            Message readMsg = handler.obtainMessage(MessageConstants.MESSAGE_READ, numBytes, -1, mmBuffer);
            Log.v("HaskellActivity", "sending message to target...");
            readMsg.sendToTarget();
            Log.v("HaskellActivity", "message sent.");
          }
        } catch (IOException e) {
          Log.d("HaskellActivity", ("Input stream disconnected: " + e));
          isAcceptThreadInProgress = false;
          break;
        }
      }
    }

    public void cancel() {
      try {
        mmSocket.close();
      } catch (IOException e) {
        Log.e("HaskellActivity", ("Failed to close connection socket: " + e));
        isAcceptThreadInProgress = false;
      }
    }
  }
}

