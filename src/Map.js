/* global exports */
"use strict"

exports.d3MapFn          = function(data)          { return d3.map(data); }
exports.d3MapFnFn        = function(data, fn)      { return d3.map(data, fn); }

exports.d3mapGetFn     = function(key, map)        { return map.get(key); }
exports.d3mapSetFn     = function(key, value, map) { return map.set(key, value); }
exports.d3mapHasFn     = function(key, map)        { return map.has(key); }
exports.d3mapRemoveFn  = function(key, map)        { return map.remove(key); }
exports.d3mapClearFn   = function(map)             { return map.clear(); }
exports.d3mapKeysFn    = function(map)             { return map.keys(); }
exports.d3mapValuesFn  = function(map)             { return map.values(); }
exports.d3mapEntriesFn = function(map)             { return map.entries(); }
exports.d3mapSizeFn    = function(map)             { return map.size(); }
exports.d3mapEmptyFn   = function(map)             { return map.empty(); }
exports.d3mapEachFn    = function(fn, map)         { return map.each(fn); }
