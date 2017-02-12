package systems.obsidian.focus;

import android.webkit.JavascriptInterface;
import android.webkit.WebView;

public class JSaddleShim {
  private WebView wv;

  public JSaddleShim ( WebView n_wv ) {
    wv = n_wv;
  }

  public void loadHTMLString ( String html ) {
    wv.loadData(html, "text/html; charset=utf-8", "UTF-8");
  }

  public void evaluateJavascript ( String javascript ) {
    wv.evaluateJavascript ( javascript, null );
  }

  @JavascriptInterface
  public void postMessage ( String msg ) {
    new ProcessJSaddleMessage().execute(msg);
  }

  public void startHandler () {
    new JSaddleStart().execute();
  }

}
