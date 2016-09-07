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

// custom version of mkEffFn1 which passes a row containing data including element and 'this'
// enables callbacks in the D3 style which rely on 'this' for access to the D3Element associated with the datum
exports.onImpl =  function (selection, eventType, callback) {
                    selection.on(eventType, callback);
                    return selection;
                  }
// NB we default 'prop' to duplicate of datum
exports.mkCallback = function (fn) {
                        return function(d) {
                          return fn(getCallBackParams(d, this, d))();
                        };
                      };

// another callback-making function, this time taking a property name and bundling that with
exports.onImplWithProperty = function (selection, eventType, callback, propname, prop) {
                                selection.on(eventType, callback);
                                selection.property(propname, prop);
                                return selection;
                             }
exports.mkCallbackWithProp = function mkCallbackWithProp(fn) {
                                return function(propName) {
                                  return function(d) {
                                    return fn(getCallBackParams(d, this, this[propName]))();
                                  }
                                };
                              };

// utility function to package up the parameter block for a callback
function getCallBackParams(d, elem, prop) {
  var cbParams = { datum: d
                 , elem: elem
                 , prop: prop    // NB - untyped assignment - only use in mkCallback fns
                 , timestamp: d3.event.timeStamp
                 , meta:      d3.event.metaKey
                 , shift:     d3.event.shiftKey
                 , ctrl:      d3.event.ctrlKey
                 , alt:       d3.event.altKey };
  return cbParams;
}
