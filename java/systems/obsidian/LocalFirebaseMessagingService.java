// File based on https://github.com/firebase/quickstart-android/blob/d454ecdbb664450d11812b9cfde91fca36c56a81/messaging/app/src/main/java/com/google/firebase/quickstart/fcm/java/MyFirebaseMessagingService.java
package systems.obsidian;

import android.util.Log;
import com.google.firebase.messaging.FirebaseMessagingService;
import com.google.firebase.messaging.RemoteMessage;

public class LocalFirebaseMessagingService extends FirebaseMessagingService {

  private static final String TAG = "LocalFirebaseMessagingService";

  private native void handleDeviceToken(String token);
  private native void handleNotification(String intent, String notificationdata);

  /**
    * Called if InstanceID token is updated. This may occur if the security of
    * the previous token had been compromised. Note that this is called when the InstanceID token
    * is initially generated so this is where you would retrieve the token.
    */
  @Override
  public void onNewToken(String token) {
    Log.d(TAG, "onNewToken: Refreshed token: " + token);

    // If you want to send messages to this application instance or
    // manage this apps subscriptions on the server side, send the
    // Instance ID token to your app server.
    handleDeviceToken(token);
  }

  @Override
  public void onMessageReceived(RemoteMessage remoteMessage) {
    if (remoteMessage.getData().size() > 0) {
      String custom = remoteMessage.getData().get("custom");
      if (custom != null) {
        handleNotification("custom-foreground", custom);
      }
    }
  }
}
