'use strict';

exports['foldl\''] = function(peek) {
  return function(length) {
    return function(f) {
      return function(z) {
        return function(xs) {
          var r = z;
          for (var i = 0; i < length; ++i) {
            var x = peek(i, xs)();
            r = f(r)(x);
          }
          return r;
        };
      };
    };
  };
};

exports['foldr\''] = function(peek) {
  return function(length) {
    return function(f) {
      return function(z) {
        return function(xs) {
          var r = z;
          var i = length;
          while (--i >= 0) {
            var x = peek(i, xs)();
            r = f(x)(r);
          }
          return r;
        };
      };
    };
  };
};
