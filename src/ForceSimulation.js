/* global exports */
"use strict"

exports.addForceFn          = function(forcetype, force, simulation) { return simulation.force(forcetype, force); }
exports.d3ForceSimulationFn = function()            { return d3.forceSimulation(); }
exports.makeCenterForceFn   = function()            { return d3.forceCenter(); }
exports.makeCenterForceFnP  = function(xy)          { return d3.forceCenter(xy); }
exports.makeLinkForceFn     = function(links)       { return d3.forceLink(links);  }
exports.makeLinkForceFnFn   = function(links, idFn) { return d3.forceLink(links, idFn); }
exports.makeManyBodyForceFn = function()            { return d3.forceManyBody(); }
exports.simulationNodesFn   = function(nodes, simulation) { return simulation.nodes(nodes); }

exports.onTickFn            = function(tick, simulation)  { return simulation.on("tick", tick); }
