/* global exports */
"use strict";

exports.mkTweenTargetEffFn   = InterpolateUncurry
exports.mkTweenFunctionEffFn = InterpolateUncurry


function InterpolateUncurry (fn) {
  return function (datum) {
    return function (index) {
      return function (elem) {
          return fn(datum, index, elem);  // this is what the javascript calls
      }
    }
  }
}
