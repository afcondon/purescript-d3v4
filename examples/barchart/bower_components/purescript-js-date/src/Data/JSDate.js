/* global exports */
"use strict";

exports.isValid = function (date) {
  return !isNaN(date.getTime());
};

exports.toInstantImpl = function (just) {
  return function (nothing) {
    return function (date) {
      var t = date.getTime();
      return isNaN(t) ? nothing : just(t);
    };
  };
};

exports.jsdate = function (parts) {
  var t = Date.UTC(parts.year, parts.month, parts.day, parts.hour, parts.minute, parts.second, parts.millisecond);
  return new Date(t);
};

exports.jsdateLocal = function (parts) {
  return function () {
    return new Date(parts.year, parts.month, parts.day, parts.hour, parts.minute, parts.second, parts.millisecond);
  };
};

exports.dateMethod = function (method, date) {
  return date[method]();
};

exports.dateMethodEff = function (method, date) {
  return function () {
    return date[method]();
  };
};

exports.parse = function (dateString) {
  return function () {
    return new Date(dateString);
  };
};
