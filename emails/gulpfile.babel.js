import gulp     from 'gulp';
import plugins  from 'gulp-load-plugins';
import browser  from 'browser-sync';
import rimraf   from 'rimraf';
import panini   from 'panini';
import yargs    from 'yargs';
import lazypipe from 'lazypipe';
import inky     from 'inky';
import fs       from 'fs';
import siphon   from 'siphon-media-query';
import path     from 'path';
import merge    from 'merge-stream';
import beep     from 'beepbeep';
import colors   from 'colors';

const $ = plugins();

// Look for the --production flag
const PRODUCTION = !!(yargs.argv.production);

// Host name - change to the correct one for production
const hostName = 'http://localhost:32768';


// Compile Sass into CSS, to be inlined
// Backend just needs it for inlining
// Dev also uses the generated css file for quick development
function sass() {
  return gulp.src('src/assets/scss/app.scss')
    .pipe($.if(!PRODUCTION, $.sourcemaps.init()))
    .pipe($.sass({
      includePaths: ['node_modules/foundation-emails/scss']
    }).on('error', $.sass.logError))
    .pipe($.if(!PRODUCTION, $.sourcemaps.write()))
    .pipe(gulp.dest('dist/css'));
}
gulp.task('sass', sass);

// We don't use Panini for the Backend
// All template stuff is handled by haskell mustache
function runinky() {
  return gulp.src('src/*.html')
    .pipe(inky())
    // Remove the data block at the top
    .pipe($.replace(/^---\n[\s\S]*\n---\n/, ''))
    // Make image urls global
    .pipe($.replace(/=('|")(\/?assets\/img)/g, "=$1"+ hostName + "/assets/img"))
    .pipe(gulp.dest('dist'));
}
gulp.task('runinky', ['sass'], runinky);

// For dev
// Compile pages, and partials into flat HTML files
// Then parse using Inky templates
function pages() {
  // Reset Panini's cache of partials
  panini.refresh();
  // Build
  return gulp.src('src/*.html')
    .pipe(panini({
      root: 'src',
      layouts: 'src/layouts',
      partials: 'src'
    }))
    .pipe(inky())
    .pipe(gulp.dest('dist'));
}
gulp.task('pages', ['sass'], pages);

// Backend
// Inline CSS and minify HTML
function inline() {
  return gulp.src('dist/*.html')
    .pipe($.if(PRODUCTION, inliner('dist/css/app.css')))
    .pipe(gulp.dest('prebuild'));
}
gulp.task('inline',['sass', 'runinky'], inline);

// Dev
// Start a server with LiveReload to preview the site in
function server() {
  browser.init({
    server: 'dist',
    port: 8000
  });
}
gulp.task('server', ['pages'], server);

// Inlines CSS into HTML, adds media query CSS into the <style> tag of the email, and compresses the HTML
function inliner(css) {
  var css = fs.readFileSync(css).toString();
  var mqCss = siphon(css);

  var pipe = lazypipe()
    .pipe($.inlineCss, {
      applyStyleTags: false,
      removeStyleTags: false,
      removeLinkTags: false
    })
    .pipe($.replace, '<!-- <style> -->', `<style>${mqCss}</style>`)
    .pipe($.htmlmin, {
      collapseWhitespace: true,
      minifyCSS: true
    });

  return pipe();
}


// Watch for file changes
gulp.task('watch:reload', ['pages'], function() { browser.reload(); });
gulp.task('watch:all', function watchall() {
  gulp.watch(['src/*.html', 'src/assets/scss/**/*.scss'], ['watch:reload']);
});

// Build emails, run the server, and watch for file changes
gulp.task('default', ['server', 'watch:all']);

// Build for backend
gulp.task('build', ['inline']);
