/* global exports */
"use strict";

exports.d3DragFn    = function() { return d3.drag(); }
exports.d3DragEvent = function() { return d3.event(); }

/******************************** wrappers for drag.on()   **********************************/

// When a specified event is dispatched, each listener will be invoked with the
// same context and arguments as selection.on listeners: the current datum d and
// index i, with the this context as the current DOM element.

// If listener is not specified, returns the first currently-assigned listener
// matching the specified typenames, if any.
exports.findCallbackFn    = function(typenames, drag) { return drag.on(typenames); }

// If listener is null, removes the current event listeners for the specified
// typenames, if any.
exports.removeListenersFn = function(typenames, drag) { return drag.on(typenames, null); }

// If listener is specified, sets the event listener for the specified typenames
// and returns the drag behavior. If an event listener was already registered for
// the same type and name, the existing listener is removed before the new listener
// is added.
exports.addListenerFn     = function(typenames, listener, drag) {
  return drag.on(typenames, listener); }

// i think it's obvious that relying on these state changes to
exports.dragUpdateFn      = function(datum, element) {
  d3.select(element)
    .attr("cx", datum.x = d3.event.x)
    .attr("cy", datum.y = d3.event.y)
}

// exports.applyDragFn       = function(drag, selection) {
//   return selection.call(drag)(); }

exports.mkEffFn4Special   = function mkEffFn4Special(fn) {
  return function(d, i, els) {
    var el = this;
    var datum = d, index = i, elements = els;
    return fn(datum)(index)(elements)(el)();
  };
};
