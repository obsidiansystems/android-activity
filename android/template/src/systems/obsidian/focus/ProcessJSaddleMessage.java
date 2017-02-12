package systems.obsidian.focus;

import android.os.AsyncTask;
import android.util.Log;

public class ProcessJSaddleMessage extends AsyncTask <String, Void, Void> {
  @Override
  protected Void doInBackground (String... msgs) {
    for (String msg : msgs) {
      Log.v ("TEST", msg);
      processMessageShim (msg);
    }
    return null;
  }

  private native void processMessageShim ( String msg );
}
