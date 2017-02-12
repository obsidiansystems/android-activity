package systems.obsidian.focus;

import android.os.AsyncTask;

public class JSaddleStart extends AsyncTask <Void, Void, Void> {
  @Override
  protected Void doInBackground (Void... unused) {
    startHandlerShim();
    return null;
  }

  private native void startHandlerShim ();
}
