/* global exports */
"use strict";

exports.d3TransitionFn    = function (name)              { return d3.transition(name); }

exports.transitionFn      = function (selection)       { return selection.transition(); }
exports.namedTransitionFn = function (name, selection) { return selection.transition(name); }
exports.savedTransitionFn = function (t, selection)    { return selection.transition(t); }
exports.durationFn        = function (ms, transition)  { return transition.duration(ms); }
exports.attrFn            = function (name, value, transition)  { return transition.attr(name, value); }
exports.attrIFn           = function (name, interp, transition) { return transition.attr(name, interp); }
exports.mergeFn           = function (other, transition)        { return transition.merge(other); }
exports.nodeFn            = function (transition)               { return transition.node(); }
exports.nodesFn           = function (transition)               { return transition.nodes(); }
exports.styleFn           = function (name, value, transition)  { return transition.style(name, value); }
exports.styleIFn          = function (name, interp, transition) { return transition.style(name, interp); }
exports.styleTweenFn      = function (name, interp, transition) { return transition.styleTween(name, interp); }
