'use strict';

function go(input, output, inputOffset, outputOffset, length, stride) {
  if (length === 1) {
    output[outputOffset] = input[inputOffset];
    output[outputOffset + 1] = 0.0;
    return;
  }

  go(input, output, inputOffset, outputOffset, length / 2, 2 * stride);
  go(input, output, inputOffset + stride, outputOffset + length, length / 2, 2 * stride);

  for (var k = 0; k < length / 2; ++k) {
    var localOffset1 = outputOffset + 2 * k;
    var localOffset2 = outputOffset + 2 * k + length;
    var θ = -2.0 * Math.PI * k / length;
    var cosθ = Math.cos(θ);
    var sinθ = Math.sin(θ);
    var tfReal = cosθ * output[localOffset2]
               - sinθ * output[localOffset2 + 1];
    var tfImag = cosθ * output[localOffset2 + 1]
               + sinθ * output[localOffset2];
    var oldReal1 = output[localOffset1];
    var oldImag1 = output[localOffset1 + 1];
    var newReal1 = oldReal1 + tfReal;
    var newImag1 = oldImag1 + tfImag;
    var newReal2 = oldReal1 - tfReal;
    var newImag2 = oldImag1 - tfImag;
    output[localOffset1] = newReal1;
    output[localOffset1 + 1] = newImag1;
    output[localOffset2] = newReal2;
    output[localOffset2 + 1] = newImag2;
  }
}

exports.fft64 = function(input) {
  return function(output) {
    return function() {
      go(input, output, 0, 0, input.length, 1);
    };
  };
};
