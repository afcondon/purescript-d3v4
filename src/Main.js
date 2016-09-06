/* global exports */
"use strict";
exports.rootSelectImpl    = rootSelect
exports.selectAllImpl     = selectAll
exports.bindDataImpl      = bindData
exports.enterImpl         = enter
exports.unsafeStyleImplP  = unsafeStyleP
exports.unsafeTextImplP   = unsafeTextP
exports.unsafeInsertImpl  = unsafeInsert
exports.unsafeAppendImpl  = unsafeAppend

// functions for typeclass "Appendable"
function unsafeInsert(selector, selection) {
  return selection.insert(selector);
}
function unsafeAppend(tag, selection) {
  return selection.append(tag);
}

function rootSelect(selector) {
  return d3.select(selector);
}
function selectAll(selector, selection) {
  return selection.selectAll(selector);
}
function bindData(array, selection) {
  return selection.data(array);
}
function enter(update) {
  return update.enter();
}
function unsafeStyleP(key, val, selection) {
  return selection.style(key, val);
}
function unsafeTextP(text, selection) {
  return selection.text(text);
}
