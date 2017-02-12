package systems.obsidian.focus;

import android.app.Activity;
import android.os.Bundle;
import android.webkit.WebView;
import android.webkit.WebSettings;
import android.view.Window;
import android.view.WindowManager;

import android.os.SystemClock;

public class MainActivity extends Activity
{

    static {
        System.loadLibrary("jsaddle-shim");
    }
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        // Remove title and notification bars, obv.
        this.requestWindowFeature(Window.FEATURE_NO_TITLE);
        this.getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN);

        setContentView(R.layout.main);
        // find the web view
        WebView wv = (WebView) findViewById (R.id.webview);
        // enable javascript
        WebSettings ws = wv.getSettings();
        ws.setJavaScriptEnabled(true);
        // init an object managing the JSaddleShim
        JSaddleShim jsaddle = new JSaddleShim(wv);
        wv.addJavascriptInterface(jsaddle, "jsaddle");
        // create and set a web view client aware of the JSaddleShim
        JSaddleWebViewClient wv_client = new JSaddleWebViewClient(jsaddle);
        wv.setWebViewClient(wv_client);

        jsaddle.loadHTMLString ("<!DOCTYPE html><html><head><title>JSaddle</title></head><body></body></html>");
        jsaddle.evaluateJavascript("jsaddle.postMessage(\"HELLO WORLD\")");
    }
}

// wv.loadUrl("file:///android_asset/frontend.jsexe/index.html");
