package systems.obsidian;

import android.app.Activity;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothManager;
import android.bluetooth.BluetoothServerSocket;
import android.bluetooth.BluetoothSocket;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.os.ParcelUuid;
import android.util.Log;
import android.webkit.ValueCallback;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.Thread;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

/**
* Library of convenient bluetooth functions.
* This class embodies all the functions necessary to establish and RFComm connection between an APK
* and another client/server via bluetooth.
*/
public class BluetoothLib {

  private Handler handler; //fetches Bluetooth service info
  private int nextRequestCode = 0;
  private ValueCallback<Uri[]> fileUploadCallback;
  private boolean isAcceptThreadInProgress = false;
  public ArrayList<String> discoveredDevices = new ArrayList<String>();
  public ArrayList<BluetoothDevice> connectionReadyDevices = new ArrayList<BluetoothDevice>();
  // Outputstream in order to write to connected devices. Has been set at this level in order to be accessed
  // by Reflex via FFI imports to pass strings from input dom element to connected bluetooth device
  private OutputStream mmOutStream;
  private byte[] mmBuffer = new byte[1024];
  private BluetoothServerSocket mmServerSocket;

	/**
	* Enables bluetooth.
	* This method will attempt to enable bluetooth if it is not already enabled
	*
	* @param  ctx  Application Context
	* @return      void
	* @see         BluetoothAdapter
	*/
  public void enableBluetooth(Context ctx) {
    BluetoothAdapter bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
    if (bluetoothAdapter == null) {
      Log.v("BluetoothLib", "bluetoothAdapter is null");
    } else {
      Log.v("BluetoothLib", "bluetoothAdapter obtained");
    }

    //Enable bluetooth if it isn't already enabled
    if (!bluetoothAdapter.isEnabled()) {
      Log.v("BluetoothLib", "Enabling bluetooth...");
      Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
      ctx.startActivity(enableBtIntent);
      Log.v("BluetoothLib", "...bluetooth has been enabled.");
    } else {
      Log.v("BluetoothLib", "Bluetooth enabled");
    }
  }


	/**
	* Scans for paired devices.
	* This method will retreive a set of paired devices.
  *
	* @param  ctx  Application Context
	* @return      Set<BluetoothDevice>
	* @see         BluetoothAdapter.getBondedDevices
	*/
  public Set<BluetoothDevice> scanDevices(Context ctx) {
    BluetoothAdapter bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
    if (bluetoothAdapter == null) {
      Log.v("BluetoothLib", "bluetoothAdapter is null");
    } else {
      Log.v("BluetoothLib", "bluetoothAdapter obtained");
    }

    //Enable bluetooth if it isn't already enabled
    if (!bluetoothAdapter.isEnabled()) {
      Log.v("BluetoothLib", "Enabling bluetooth...");
      Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
      ctx.startActivity(enableBtIntent);
      Log.v("BluetoothLib", "...bluetooth has been enabled.");
    } else {
      Log.v("BluetoothLib", "Bluetooth enabled");
    }

    // Clear list before searching to avoid duplicate results
    connectionReadyDevices.clear();

    Set<BluetoothDevice> pairedDevices = bluetoothAdapter.getBondedDevices();
    for (BluetoothDevice bt : pairedDevices) {
      connectionReadyDevices.add(bt);
    }

    return pairedDevices;
  }

	/**
	* Discover available devices.
	* This method will retreive a list of discovered devices, delimit the device name and mac address with
	* a '|' and add it to a list of connection ready devices.
	*
	* @param  ctx  Application Context
	* @return      void
	* @see         BluetoothAdapter.startDiscovery
	*/
  public void discoverDevices(Context ctx) {
    BluetoothAdapter bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
    if (bluetoothAdapter == null) {
      Log.v("BluetoothLib", "bluetoothAdapter is null");
    } else {
      Log.v("BluetoothLib", "bluetoothAdapter obtained");
    }

    //Enable bluetooth if it isn't already enabled
    if (!bluetoothAdapter.isEnabled()) {
      Log.v("BluetoothLib", "Enabling bluetooth...");
      Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
      ctx.startActivity(enableBtIntent);
      Log.v("BluetoothLib", "...bluetooth has been enabled.");
    } else {
      Log.v("BluetoothLib", "Bluetooth enabled");
    }

    // Clear list before searching to avoid duplicate results
    discoveredDevices.clear();
    connectionReadyDevices.clear();

    IntentFilter filter = new IntentFilter(BluetoothDevice.ACTION_FOUND);
    ctx.registerReceiver(receiver, filter);
    Log.v("BluetoothLib", "Receiver registered");
    // Pre-check to make sure device is not already discovering
    if (bluetoothAdapter.isDiscovering()) {
      bluetoothAdapter.cancelDiscovery();
    }
    bluetoothAdapter.startDiscovery();
    // Give discovery a moment to start up
    try {
      Thread.sleep(2000);
    } catch(InterruptedException e) {
      Log.v("BluetoothLib", "Thread.sleep exception thrown");
    }
    if (bluetoothAdapter.isDiscovering()) {
      Log.v("BluetoothLib", "Discovery started...");
      // Give 10 seconds for discovery to discover devices
      try {
        Thread.sleep(10000);
      } catch(InterruptedException e) {
        Log.v("BluetoothLib", "Thread.sleep exception thrown");
      }
      Log.v("BluetoothLib", "Cancelling discovery...");
      boolean discoveryDisabled = bluetoothAdapter.cancelDiscovery();
      if (discoveryDisabled) {
        Log.v("BluetoothLib", "Discovery cancelled");
      } else {
        Log.v("BluetoothLib", "Discovery cancellation unsuccessful");
      }
    } else {
      /* Note: If bluetooth adapter never started,
      ** it's likely that location is not enabled by the user.
      ** Check within Settings -> App -> "this app name" ->
      ** permissions on the android device */
      Log.v("BluetoothLib", "Discovery was not started.");
    }
  }

  // Specify what actions should take place when a new device is discovered
  private final BroadcastReceiver receiver = new BroadcastReceiver() {
    public void onReceive(Context context, Intent intent) {
      String action = intent.getAction();
      if (BluetoothDevice.ACTION_FOUND.equals(action)) {
        BluetoothDevice device = intent.getParcelableExtra(BluetoothDevice.EXTRA_DEVICE);
        String deviceName = device.getName();
        String deviceHardwareAddress = device.getAddress();
        Log.v("BluetoothLib", ("Discovered the following device: " + deviceName));

        discoveredDevices.add(deviceName + "|" + deviceHardwareAddress);
        connectionReadyDevices.add(device);
      }
    }
  };

	/**
	* Fetch list of connection ready devices.
	* This method will retreive a list of devices that are ready to connect.
  * This method is best used AFTER calling discoverDevices()
	*
	* @param  ctx  Application Context
	* @return      Set<BluetoothDevice>
  * @see         BluetoothDevice
	*/
  public ArrayList<BluetoothDevice> getAvailableBluetoothDevices() {
    return connectionReadyDevices;
  }

  // Establish bluetooth communication threads.
  // Note: Requires acceptance from Linux client/server
	/**
	* Establish radio frequency communication with specified bluetooth device.
	* This method will initiate communication with a speficied bluetoth device and establish
  * necessary input and output streams of communication with specified device. Once called,
  * the function will wait forever in a parrallel thread to be accepted by the device specified
  * until app cancellation or application termination.
  *
	* @param  ctx  String Bluetooth MAC Address
	* @return      String Connection Status
	* @see         AcceptThread
	*/
  public String establishRFComm(String btDeviceName) {
    String status = null;
    if (! isAcceptThreadInProgress) {
      isAcceptThreadInProgress = true;
      AcceptThread acceptThread = new AcceptThread(btDeviceName);
      Log.v("BluetoothLib", ("Establishing bluetooth communication with " + btDeviceName + "..."));
      try {
        Thread.sleep(1000);
      } catch(InterruptedException e) {
        Log.v("BluetoothLib", "Thread.sleep exception thrown");
        status = "ThreadSleepError";
      }
      acceptThread.start();
      status = "Connected";
    } else {
      Log.v("BluetoothLib", ("AcceptThread already in progress..."));
      status = "DuplicateConnectionError";
    }

    return status.toString();
  }

	/**
	* Write to connected device.
	* This method will write the argument specified string to the established RFComm.
  * This method works assuming a successful connection has been initiated using establishRFComm()
	* @param  byte[] input
	* @return        void
	* @see           establishRFComm
	*/
  public void writeToConnectedDevice(byte[] bytes) {
    try {
      mmOutStream.write(bytes);
    } catch (IOException e){
      Log.e("BluetoothLib", ("Error while sending data:" + e));
      Bundle bundle = new Bundle();
      bundle.putString("toast", "Data could not be sent to other device");
    }
  }

	/**
	* Cancel/Disconnect from any established or in progress bluetooth connections.
	* This method will write the argument specified string to the established RFComm.
  * This method works assuming a successful connection has been initiated using establishRFComm()
	* @param  ctx  Application Context
	* @return      void
	* @see         ServerSocket.close
	*/
  public void cancelBluetoothConnection(Context ctx) {
    if (isAcceptThreadInProgress) {
      try {
        mmServerSocket.close();
      } catch (IOException e) {
        Log.e("BluetoothLib", ("Failed to close connection socket: " + e));
        isAcceptThreadInProgress = false;
      }
      ctx.unregisterReceiver(receiver);
    }
  }

  private interface MessageConstants {
    public static final int MESSAGE_READ = 0;
    public static final int MESSAGE_WRITE = 1;
  }

  class AcceptThread extends Thread {

    public BluetoothDevice getBluetoothDeviceInfo(String dvAddr, ArrayList<BluetoothDevice> dvs) {
      if (! dvs.isEmpty()) {
        Log.v("BluetoothLib", ("getBluetoothDeviceInfo: LOOKING FOR " + dvAddr));
        for (BluetoothDevice dv : dvs) {
          Log.v("BluetoothLib", ("LOOKING AT " + dv.getName() + " with mac address " + dv.getAddress()));
          if (dv.getAddress().equals(dvAddr)) {
            Log.v("BluetoothLib", "bluetoothDevice hardware address found");
            return dv;
          }
        }
        Log.v("BluetoothLib", "bluetoothDevice hardware address not found");
        return null;
      }
      Log.v("BluetoothLib", "empty list was passed to getBluetoothDeviceInfo");
      return null;
    }

    public AcceptThread(String deviceAddr) {
      BluetoothServerSocket tmp = null;

      BluetoothAdapter bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
      if (bluetoothAdapter == null) {
        Log.v("BluetoothLib", "bluetoothAdapter is null");
      } else {
        Log.v("BluetoothLib", "bluetoothAdapter obtained");
      }

      Log.v("BluetoothLib", "Getting Device info....");
      // Retreive bluetooth device information for use when establishing RFComm
      BluetoothDevice btDevice = getBluetoothDeviceInfo(deviceAddr, connectionReadyDevices);
      Log.v("BluetoothLib", "Getting UUID info....");
      ParcelUuid[] uuids = btDevice.getUuids();
      if (!(uuids.length <= 0)) {
        Log.v("BluetoothLib", "UUID info obtained.");

        try {
          Log.v("BluetoothLib", ("listenUsingRfcommWithServiceRecord initiating with UUID: " + uuids[0].getUuid()));
          tmp = bluetoothAdapter.listenUsingRfcommWithServiceRecord(btDevice.getName(), uuids[0].getUuid());
        } catch (IOException e) {
          Log.e("BluetoothLib", ("Socket listen failed" + e));
          isAcceptThreadInProgress = false;
        }
        Log.v("BluetoothLib", "setting mmServerSocket...");
        mmServerSocket = tmp;
      } else {
        Log.e("BluetoothLib", "Bluetooth Device object not found.");
        mmServerSocket = null;
      }
    }

    public void run () {
      BluetoothSocket socket = null;

      while (true) {
        try {
          Log.v("BluetoothLib", "accepting server socket...");
          socket = mmServerSocket.accept();
          Log.v("BluetoothLib", "server socket accepted.");
        } catch (IOException e) {
          Log.e("BluetoothLib", ("Socket accept failed: " + e));
          isAcceptThreadInProgress = false;
          break;
        }

        if (socket != null) {
          Log.v("BluetoothLib", "connecting thread...");
          ConnectedThread connectedThread = new ConnectedThread(socket);
          Log.v("BluetoothLib", "running thread...");
          try {
            Thread.sleep(1000);
          } catch(InterruptedException e) {
            Log.v("BluetoothLib", "Thread.sleep exception thrown");
          }
          connectedThread.start();
          try {
            mmServerSocket.close();
          } catch (IOException e) {
            Log.e("BluetoothLib", ("Failed to close server socket: " + e));
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
        Log.e("BluetoothLib", ("Failed to close connection socket: " + e));
        isAcceptThreadInProgress = false;
      }
    }
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
        Log.v("BluetoothLib", "getting InputStream...");
        tmpIn = socket.getInputStream();
      } catch (IOException e) {
        Log.e("BluetoothLib", ("Error while creating input stream: " + e));
        isAcceptThreadInProgress = false;
      }
      try{
        Log.v("BluetoothLib", "getting OutputStream...");
        tmpOut = socket.getOutputStream();
      } catch (IOException e) {
        Log.e("BluetoothLib", ("Error while creating output stream: " + e));
        isAcceptThreadInProgress = false;
      }

      Log.v("BluetoothLib", "steams secured, setting streams");
      mmInStream = tmpIn;
      mmOutStream = tmpOut;
    }

    public void run() {
      int numBytes;

      Log.v("BluetoothLib", "ready to read incoming bytes...");

      while (true) {
        try {
          if (mmBuffer!= null) {
            Log.v("BluetoothLib", "reading bytes...");
            numBytes = mmInStream.read(mmBuffer);
            Message readMsg = handler.obtainMessage(MessageConstants.MESSAGE_READ, numBytes, -1, mmBuffer);
            Log.v("BluetoothLib", "sending message to target...");
            readMsg.sendToTarget();
            Log.v("BluetoothLib", "message sent.");
          }
        } catch (IOException e) {
          Log.d("BluetoothLib", ("Input stream disconnected: " + e));
          isAcceptThreadInProgress = false;
          break;
        }
      }
    }

    public void cancel() {
      try {
        mmSocket.close();
      } catch (IOException e) {
        Log.e("BluetoothLib", ("Failed to close connection socket: " + e));
        isAcceptThreadInProgress = false;
      }
    }
  }
}
