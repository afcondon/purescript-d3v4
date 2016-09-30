/* global exports */
"use strict"

// constructors - continuous scales
exports.d3IdentityScaleFn = function() { return d3.scaleIdentity(); }
exports.d3LinearScaleFn   = function() { return d3.scaleLinear(); }
exports.d3LogScaleFn      = function() { return d3.scaleLog(); }
exports.d3PowerScaleFn    = function() { return d3.scalePow(); }
exports.d3TimeScaleFn     = function() { return d3.scaleTime(); }

// constructors - ordinal scales
exports.d3BandScaleFn     = function()       { return d3.scaleBand(); }
exports.d3PointScaleFn    = function()       { return d3.scalePoint(); }
exports.d3CategoryScaleFn = function(scheme) { return d3.scaleCategory(scheme); }

exports.


// functions on continuous scales
exports.rangeRoundFn      = function(start, end, scale)    { return scale.rangeRound([start, end]); }
exports.applyScaleFn      = function(d, scale)             { return scale(d); }

// these functions are particular to Band scales
exports.paddingFn         = function(p, scale) { return scale.padding(p); }
exports.paddingInnerFn    = function(p, scale) { return scale.paddingInner(p); }
exports.paddingOuterFn    = function(p, scale) { return scale.paddingOuter(p); }
exports.bandwidthFn       = function(scale)    { return scale.bandwidth(); }

// interpolator constructors
exports.d3InterpolateViridisFn = function() { return d3.interpolateViridis() }  //  a dark-to-light color scheme.
exports.d3InterpolateInfernoFn = function() { return d3.interpolateInferno() }  //  a dark-to-light color scheme.
exports.d3InterpolateMagmaFn = function() { return d3.interpolateMagma() }  //  a dark-to-light color scheme.
exports.d3InterpolatePlasmaFn = function() { return d3.interpolatePlasma() }  //  a dark-to-light color scheme.
exports.d3InterpolateWarmFn = function() { return d3.interpolateWarm() }  //  a rotating-hue color scheme.
exports.d3InterpolateCoolFn = function() { return d3.interpolateCool() }  //  a rotating-hue color scheme.
exports.d3InterpolateRainbowFn = function() { return d3.interpolateRainbow() }  //  a cyclical rotating-hue color scheme.
exports.d3InterpolateCubehelixDefaultFn = function() { return d3.interpolateCubehelixDefault() }  //  a dark-to-light, rotating-hue color scheme.
