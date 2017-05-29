## Installing fonts locally

1. Download from http://www.localfont.com/
1. Unzip in `/static`; WARNING: this will overwrite `/static/css/fonts.css` and some other files
1. In `/static/css/fonts.css`, replace `url('/` with `url('../`.  This is necessary to make Android (at least) work - we're served out of file:///android_asset/ there, so / goes to file:///, which doesn't point at our static files.
