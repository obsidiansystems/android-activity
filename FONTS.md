## Installing fonts locally

1. Download font(s) of your choice from [google-webfonts-helper.herokuapp.com/fonts](https://google-webfonts-helper.herokuapp.com/fonts) 
1. Unzip in `/static` or create a `static/fonts` directory and unzip there
1. In `/static/css/fonts.css`, replace `url('/` with `url('../`.  This is necessary to make Android (at least) work - we're served out of file:///android_asset/ there, so / goes to file:///, which doesn't point at our static files.

