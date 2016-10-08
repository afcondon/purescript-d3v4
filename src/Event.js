/* global exports */
"use strict";

exports.d3MouseFn = function(container) { return d3.mouse(container); }
exports.d3EventFn = function()          { return d3.event; }

// Returns the x and y coordinates of the touch with the specified identifier
// associated with the current event relative to the specified container. The
// container may be an HTML or SVG container element, such as a G element or an
// SVG element. The coordinates are returned as an array of two-element arrays
// of numbers [[x1, y1], [x2, y2], …]. If there is no touch with the specified
// identifier in touches, returns null; this can be useful for ignoring
// touchmove events where the only some touches have moved. If touches is not
// specified, it defaults to the current event’s changedTouches property.
exports.d3TouchFn        = function(container, touches, id) { return d3.touch(container, touches, id); }
exports.d3TouchDefaultFn = function(container, id)          { return d3.touch(container, id); }

// Returns the x and y coordinates of the touches associated with the current
// event relative to the specified container. The container may be an HTML or
// SVG container element, such as a G element or an SVG element. The coordinates
// are returned as an array of two-element arrays of numbers [[x1, y1], [x2,
// y2], …]. If touches is not specified, it defaults to the current event’s
// touches property.
exports.d3TouchesFn         = function(container, touches) { return d3.touches(container, touches); }
exports.d3TouchesDefaultFn  = function(container)          { return d3.touches(container); }
