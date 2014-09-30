var gulp = require('gulp'),
    purescript = require('gulp-purescript');

var src = [
  'bower_components/purescript-*/src/**/*.purs',
  'src/**/*.purs'
];

var compile = function(options) {
  return function() {
    return gulp.src(src)
      .pipe(purescript.psc({main: true, output: 'app.js'}))
      .pipe(gulp.dest('build'));
  };
};

gulp.task('src', compile({}));

gulp.task('watch', function() {
  gulp.watch("src/**/*", ['src']);
});

gulp.task('default', ['src', 'watch']);
