/* global exports */
"use strict"

exports.d3ForceSimulationFn = function() { return d3.forceSimulation(); }
exports.addLinkForceFn      = function() { d3.forceLink(); }

exports.makeLinkForceFnFn   = function(links, idFn) { return d3.forceLink(links, idFn); }
exports.makeLinkForceFn     = function(links)       { return d3.forceLink(links);  }

exports.makeManyBodyForceFn = function() { return d3.forceManyBody(); }

exports.makeCenterForceFnP  = function(xy) { return d3.forceCenter(xy); }
exports.makeCenterForceFn   = function() { return d3.forceCenter(); } 
