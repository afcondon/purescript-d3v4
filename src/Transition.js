/* global exports */
"use strict";

exports.d3TransitionFn    = function (name)                     { return d3.transition(name); }
exports.d3TransitionFn2   = function ()                         { return d3.transition(); }

exports.attrFn            = function (name, value, transition)  { return transition.attr(name, value); }
exports.attrIFn           = function (name, interp, transition) { return transition.attr(name, interp); }
exports.durationFn        = function (ms, transition)           { return transition.duration(ms); }
exports.emptyFn           = function (transition)               { return transition.empty(); }
exports.mergeFn           = function (other, transition)        { return transition.merge(other); }
exports.namedTransitionFn = function (name, selection)          { return selection.transition(name); }
exports.nodeFn            = function (transition)               { return transition.node(); }
exports.nodesFn           = function (transition)               { return transition.nodes(); }
exports.savedTransitionFn = function (t, selection)             { return selection.transition(t); }
exports.styleFn           = function (name, value, transition)  { return transition.style(name, value); }
exports.styleIFn          = function (name, interp, transition) { return transition.style(name, interp); }
exports.styleTweenFn      = function (name, interp, transition) { return transition.styleTween(name, interp); }
exports.transitionFn      = function (selection)                { return selection.transition(); }
exports.transition2Fn     = function (transition)               { return transition.transition(); }
exports.delayFn           = function (ms, transition)           { return transition.delay(ms); }
exports.delayIFn          = function (fn, transition)           { return transition.delay(fn); }
