const gulp = require('gulp');
const plugins = require('gulp-load-plugins');
const browser = require('browser-sync');
const rimraf = require('rimraf');
const panini = require('panini');
const yargs = require('yargs');
const lazypipe = require('lazypipe');
const inky = require('inky');
const fs = require('fs');
const siphon = require('siphon-media-query');
const path = require('path');
const merge = require('merge-stream');
const beep = require('beepbeep');
const colors = require('colors');
const runSequence = require('run-sequence');

gulp.series = function() {
  const outerArgs = arguments;
  return function(callback) {
    const runSequenceArgs = Array.prototype.slice.call(outerArgs);
    runSequenceArgs.push(callback);
    runSequence.apply(this, runSequenceArgs);
  };
};

const $ = plugins();

// Look for the --production flag
const PRODUCTION = !!(yargs.argv.production);

// Host name - change to the correct one for production
const hostName = 'http://localhost:32768';

// Build emails, run the server, and watch for file changes
gulp.task('default', gulp.series("clean", "pages", "images", "sass", server, watch));

// Build for backend
gulp.task('build', gulp.series(clean, runinky, images, sass, inline));

// This happens every time a build starts
function clean(done) {
  rimraf('dist', done);
  rimraf('prebuild', done);
}

// We don't use Panini for the Backend
// All template stuff is handled by haskell mustache
function runinky() {
  return gulp.src('src/**/*.html')
    .pipe(inky())
    // Remove the data block on the top
    .pipe($.replace(/^---\n.*\n---\n/, ''))
    // Make image urls global
    .pipe($.replace(/=('|")(\/?assets\/img)/g, "=$1"+ hostName + "/assets/img"))
    .pipe(gulp.dest('prebuild'));
}

// For dev
// Compile pages, and partials into flat HTML files
// Then parse using Inky templates
function pages() {
  return gulp.src('src/pages/**/*.html')
    .pipe(panini({
      root: 'src/pages',
      layouts: 'src/layouts',
      partials: 'src/partials'
    }))
    .pipe(inky())
    .pipe(gulp.dest('dist'));
}

// Reset Panini's cache of partials
function resetPages(done) {
  panini.refresh();
  done();
}

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
    .pipe(gulp.dest('dist/css'))
    .pipe(gulp.dest('prebuild/css'));
}

// Backend and Dev
// Copy and compress images
function images() {
  return gulp.src('src/assets/img/**/*')
    .pipe($.imagemin())
    .pipe(gulp.dest('./dist/assets/img'))
    .pipe(gulp.dest('./prebuild/assets/img'))
    .pipe(gulp.dest('../static/assets/img'));
}

// Backend
// Inline CSS and minify HTML
function inline() {
  return gulp.src('prebuild/**/*.html')
    .pipe($.if(PRODUCTION, inliner('dist/css/app.css')))
    .pipe(gulp.dest('prebuild'));
}

// Dev
// Start a server with LiveReload to preview the site in
function server(done) {
  browser.init({
    server: 'dist'
  });
  done();
}

// Watch for file changes
function watch() {
  gulp.watch('src/pages/**/*.html').on('change', gulp.series(pages, inline, browser.reload));
  gulp.watch(['src/partials/**/*']).on('change', gulp.series(resetPages, pages, inline, browser.reload));
  gulp.watch(['../scss/**/*.scss', 'src/assets/scss/**/*.scss']).on('change', gulp.series(resetPages, sass, pages, inline, browser.reload));
  gulp.watch('src/assets/img/**/*').on('change', gulp.series(images, browser.reload));
}

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
