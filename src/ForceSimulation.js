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

exports.defaultTickFn       = function(link, node) {
  link
  // suspect this is the core of the problem, ticked is probably called
  // immediately, ie before the simulation has started so you could either not
  // run the attrs on the first go or find out how to defer it til later
      .attr("x1", function(d) {
        if (typeof return d.source.x; })
      .attr("y1", function(d) {
        if (typeof return d.source.y; })
      .attr("x2", function(d) {
        if (typeof return d.target.x; })
      .attr("y2", function(d) {
        if (typeof return d.target.y; });

  node
      .attr("cx", function(d) {
        if (typeof return d.x; })
      .attr("cy", function(d) {
        if (typeof return d.y; });
}
