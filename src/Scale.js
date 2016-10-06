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
exports.d3CategoryScaleFn = function(scheme) { return d3.scaleOrdinal(scheme); }

// constructors - QuantizeScale
exports.d3QuantizeScaleFn  = function()      { return d3.scaleQuantize(); }
exports.d3QuantileScaleFn  = function()      { return d3.scaleQuantile(); }
exports.d3ThresholdScaleFn = function()      { return d3.ScaleThreshold(); }

// interpolator constructors
exports.d3InterpolateViridisFn = function() { return d3.interpolateViridis() }  //  a dark-to-light color scheme.
exports.d3InterpolateInfernoFn = function() { return d3.interpolateInferno() }  //  a dark-to-light color scheme.
exports.d3InterpolateMagmaFn   = function() { return d3.interpolateMagma() }    //  a dark-to-light color scheme.
exports.d3InterpolatePlasmaFn  = function() { return d3.interpolatePlasma() }   //  a dark-to-light color scheme.
exports.d3InterpolateWarmFn    = function() { return d3.interpolateWarm() }     //  a rotating-hue color scheme.
exports.d3InterpolateCoolFn    = function() { return d3.interpolateCool() }     //  a rotating-hue color scheme.
exports.d3InterpolateRainbowFn = function() { return d3.interpolateRainbow() }  //  a cyclical rotating-hue color scheme.
exports.d3InterpolateCubehelixDefaultFn = function() { return d3.interpolateCubehelixDefault() }  //  a dark-to-light, rotating-hue color scheme.

// functions on scales (not all applicable to all scale types however)
exports.rangeRoundFn  = function(start, end, scale) { return scale.rangeRound([start, end]); }
exports.applyScaleFn  = function(scale, d)          {
  return scale(d); }
exports.domainArrFn   = function(d, scale)          { return scale.domain(d); }
exports.domainMapFn   = function(d, scale)          { return scale.domain(d); }
exports.rangeFn       = function(r, scale)          { return scale.range(r); }
exports.roundFn       = function(r, scale)          { return scale.round(r); }
exports.clampFn       = function(c, scale)          { return scale.clamp(c); }
exports.interpolateFn = function(i, scale)          { return scale.interpolate(i); }
exports.niceFn        = function(scale)             { return scale.nice(); }
exports.nicePFn       = function(n, scale)          { return scale.nice(n); }          // optional parameter provided
exports.invertFn      = function(r, scale)          { return scale.invert(r); }
exports.ticksFn       = function(scale)             { return scale.ticks(); }
exports.ticksPFn      = function(n, scale)          { return scale.ticks(n); }         // optional parameter provided
exports.tickFormatFn  = function(n, scale)          { return scale.tickFormat(n); }
exports.tickFormatPFn = function(n, f, scale)       { return scale.tickFormat(n, f); } // optional parameter provided

// functions particular to Quantize scales (Quantize, Quantile, Threshold)
exports.invertExtent  = function(r, scale)          { return scale.invertExtent(r); }

// timescale ticks has a variation that takes an _interval_
// not implemented at this time
// exports.timescaleTicksFn = function(scale)          { return scale.ticks}

// applicable only to LogScale
exports.baseFn           = function(base, logscale) { return logscale.base(base); }

// applicable only to Ordinal scales
exports.unknownFn     = function() { }    // disabled for PointScale

// these functions are particular to Band scales
exports.paddingFn         = function(p, scale) { return scale.padding(p); }
exports.paddingInnerFn    = function(p, scale) { return scale.paddingInner(p); }
exports.paddingOuterFn    = function(p, scale) { return scale.paddingOuter(p); }
exports.bandwidthFn       = function(scale)    { return scale.bandwidth(); }
exports.stepFN            = function(scale)    { return scale.step(); }
exports.alignFn           = function(scale)    { return scale.align(); }

exports.schemeCategory10 = d3.schemeCategory10;
exports.schemeCategory20 = d3.schemeCategory20;
exports.schemeCategory20b = d3.schemeCategory20b;
exports.schemeCategory20c = d3.schemeCategory20c;
