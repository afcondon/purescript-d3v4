/* global exports */
"use strict"


exports.d3HierarchyFn = function(data) { return d3.hierarchy(data); }
exports.d3TreeFn      = function() { return d3.tree(); }
exports.hierarchizeFn = function(data, hierarchy) { return hierarchy(data); }
exports.treeFn        = function(data, tree) { return tree(data); }
exports.sizeFn        = function(xy, tree)   { return tree.size(xy); }
exports.separationFn  = function(f, tree)    { return tree.separation(f); }
exports.nodeSizeFn    = function(tree)   { return tree.nodeSize(); }

// functions on nodes
exports.hasChildrenFn = function(node) { return node.children ? true : false; }
exports.ancestorsFn   = function(node) { return node.ancestors(); }
exports.descendantsFn = function(node) { return node.descendants(); }
exports.leavesFn      = function(node) { return node.leaves(); }
exports.linksFn       = function(node) { return node.links(); }
exports.sumFn         = function(node) { return node.sum(); }
exports.childrenFn    = function(node) { return node.children ? node.children : [] }
exports.parentFn      = function(node) { return node.parent ? node.parent : [] }
exports.parentsEqFn   = function(a, b) {
  return a.parent == b.parent; }

// not implemented yet:
// eachFn
// eachAfterFn
// eachBeforeFn
// pathFn
// sortFn
