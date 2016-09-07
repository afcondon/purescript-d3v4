/* global exports */
"use strict";
// var Data_Tuple = require("../Data.Tuple");
exports.rootSelectImpl    = function (selector)             { return d3.select(selector); }

exports.enterImpl         = function (update)               { return update.enter(); }
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

// this bit is the call back stuff - to be revised for changes in D3v4
exports.onImpl             =
  function (selection, eventType, callback) {
    selection.on(eventType, callback);
    return selection;
  }
exports.onImplWithProperty = // variation which enables cached `prop` to be sent with callback data
  function (selection, eventType, callback, propname, prop) {
    selection.on(eventType, callback);
    selection.property(propname, prop);
    return selection;
  }

// enables callbacks in the D3 style which rely on 'this' for access to the D3Element associated with the datum
// NB we default 'prop' to duplicate of datum
exports.mkCallback = function (fn) {
  return function(d) { return fn(getCallBackParams(d, this, d))(); };
};

// another callback-making function, this time taking a property name and bundling that with
// callback params too
exports.mkCallbackWithProp = function mkCallbackWithProp(fn) {
  return function(propName) {
    return function(d) {
      var cbParams = { datum: d, elem: this
                     , prop: this[propName]
                     , timestamp: d3.event.timeStamp
                     , meta: d3.event.metaKey
                     , shift: d3.event.shiftKey
                     , ctrl: d3.event.ctrlKey
                     , alt: d3.event.altKey };
      return fn(cbParams)();
    }
  };
};

// custom version of mkEffFn1 which passes a row containing data including element and 'this'
function getCallBackParams(d, elem, prop) {
  var cbParams = { datum: d
                 , elem: elem
                 , prop: prop
                 , timestamp: d3.event.timeStamp
                 , meta:      d3.event.metaKey
                 , shift:     d3.event.shiftKey
                 , ctrl:      d3.event.ctrlKey
                 , alt:       d3.event.altKey };
  return cbParams;
}
