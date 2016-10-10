/* global exports */
"use strict";

exports.d3ZoomFn       = function() { return d3.zoom(); }
exports.scaleExtentFn  = function(extent, zoom) { return zoom.scaleExtent(extent); }
exports.d3ZoomEventFn  = function() { return d3.event; } // obvs only valid if called when zoom TODO
exports.getTransformFn = function() {
  return d3.event.transform; } // could be some Maybe stuff here

// this is the exact same implementation as that in Drag.js so it should be
// coalesced and put in Base.purs TODO
exports.addZoomListenerFn     = function(typenames, listener, zoom) {
  return zoom.on(typenames, listener); }
