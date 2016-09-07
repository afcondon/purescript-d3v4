/* global exports */
"use strict";
exports.enterImpl         = function (update)               { return update.enter(); }
exports.rootSelectImpl    = function (selector)             { return d3.select(selector); }
exports.selectAllImpl     = function (selector, selection)  { return selection.selectAll(selector); }
exports.bindDataImpl      = function (array, selection)     { return selection.data(array); }
exports.unsafeStyleImpl   = function (key, val, selection)  { return selection.style(key, val); }
exports.unsafeStyleImplP  = function (key, val, selection)  { return selection.style(key, val); }
exports.unsafeStyleImplPP = function (key, val, selection)  { return selection.style(key, function (d, i) { return val(d)(i); }) }
exports.unsafeTextImpl    = function (text, selection)      { return selection.text(text); }
exports.unsafeTextImplP   = function (text, selection)      { return selection.text(text); }
exports.unsafeTextImplPP  = function (text, selection)      { return selection.text(function (d,i) { return text(d)(i); }); }
exports.unsafeInsertImpl  = function (selector, selection)  { return selection.insert(selector); }
exports.unsafeAppendImpl  = function (tag, selection)       { return selection.append(tag); }

function rootSelect(selector) {
  return d3.select(selector);
}
