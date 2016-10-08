/* global exports */
"use strict"

exports.d3AxisTopFn    = function(scale) { return d3.axisTop(scale); }
exports.d3AxisBottomFn = function(scale) { return d3.axisBottom(scale); }
exports.d3AxisRightFn  = function(scale) { return d3.axisRight(scale); }
exports.d3AxisLeftFn   = function(scale) { return d3.axisLeft(scale); }

exports.d3AxisTicksCountFn     = function(c,axis)   { return axis.ticks(c); }
exports.d3AxisTicksCountSFn    = function(c,s,axis) { return axis.ticks(c,s); }
exports.d3AxisTicksIntervalFn  = function(c,axis)   { return axis.ticks(c); }
exports.d3AxisTicksIntervalSFn = function(c,s,axis) { return axis.ticks(c,s); }

exports.renderAxisFn           = function(axis, selection) { return selection.call(axis); }
