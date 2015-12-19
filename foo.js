var main = function() {
  var a = 1;
  var b = 2;
  var c = (function() {
    if (a < b) {
      return 0;
    } else {
      return 666;
    }
  })();

  (function() {
    console.log(c);
    return c;
  })();
};

main();
