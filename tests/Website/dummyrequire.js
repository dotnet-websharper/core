function define(modules, run) {
  var m = Array(modules.length);
  m[0] = function () { };
  m[1] = window;
  for (var i = 2; i < modules.length; i++) {
    m[i] = window;
  }
  run.apply(null, m);
}