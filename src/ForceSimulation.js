/* global exports */
"use strict"

exports.addForceFn          = function(forcetype, force, simulation) { return simulation.force(forcetype, force); }
exports.d3ForceSimulationFn = function()            { return d3.forceSimulation(); }
exports.linkIDFn            = function(f, force)    { return force.id(f); }
exports.makeCenterForceFn   = function()            { return d3.forceCenter(); }
exports.makeCenterForceFnP  = function(x,y)         { return d3.forceCenter(x,y); }
exports.makeLinkForceFn     = function(links)       { return d3.forceLink(links);  }
exports.makeManyBodyForceFn = function()            { return d3.forceManyBody(); }
exports.getLinksFn          = function(simulation)  { return simulation.links(); }
exports.setLinksFn          = function(links, force) { return force.links(links); }
exports.getForceFn          = function(name, simulation) { return simulation.force(name); }
exports.simulationNodesFn   = function(nodes, simulation) { return simulation.nodes(nodes); }

exports.onTickFn            = function(tick, simulation)  { return simulation.on("tick", tick); }

// var notFirstTime = 0;
exports.defaultTickFn       = function(node, link) {
  link
      .attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; });

  node
      .attr("cx", function(d) { return d.x; })
      .attr("cy", function(d) { return d.y; });
}
