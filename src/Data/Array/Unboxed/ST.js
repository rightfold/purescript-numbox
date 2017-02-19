'use strict';

//----------------------------------------------------------------------------//

exports.newSTUnboxedInt32Array = function(length, value) {
  if (value === 0) {
    return function() {
      return new Int32Array(length);
    };
  } else {
    return function() {
      var array = new Int32Array(length);
      array.fill(value);
      return array;
    };
  }
};

exports.lengthSTUnboxedInt32Array = function(array) {
  return array.length;
};

exports.unsafePeekSTUnboxedInt32Array = function(index, array) {
  return function() {
    return array[index];
  };
};

exports.unsafePokeSTUnboxedInt32Array = function(index, value, array) {
  return function() {
    array[index] = value;
  };
};

//----------------------------------------------------------------------------//

exports.newSTUnboxedFloat64Array = function(length, value) {
  if (value === 0.0) {
    return function() {
      return new Float64Array(length);
    };
  } else {
    return function() {
      var array = new Float64Array(length);
      array.fill(value);
      return array;
    };
  }
};

exports.lengthSTUnboxedFloat64Array = function(array) {
  return array.length;
};

exports.unsafePeekSTUnboxedFloat64Array = function(index, array) {
  return function() {
    return array[index];
  };
};

exports.unsafePokeSTUnboxedFloat64Array = function(index, value, array) {
  return function() {
    array[index] = value;
  };
};