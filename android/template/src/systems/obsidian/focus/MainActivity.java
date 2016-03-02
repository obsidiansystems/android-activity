package systems.obsidian.focus;

import android.app.Activity;
import android.os.Bundle;
import android.webkit.WebView;
import android.webkit.WebSettings;
import android.view.Window;
import android.view.WindowManager;

public class MainActivity extends Activity
{
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        // Remove title and notification bars, obv.
        this.requestWindowFeature(Window.FEATURE_NO_TITLE);
        this.getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN);

        setContentView(R.layout.main);
        // find the web view, point it at the web page, enable javascript
        WebView wv = (WebView) findViewById (R.id.webview);
        wv.loadUrl("file:///android_asset/frontend.jsexe/index.html");
        wv.setWebViewClient(new OverridenWebViewClient());
        WebSettings ws = wv.getSettings();
        ws.setJavaScriptEnabled(true);
    }
}
