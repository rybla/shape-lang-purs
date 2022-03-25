"use strict"

exports.code = function(e) {return e.code}
exports.key = function(e) {return e.key}

// exports.setNativeEventTargetProp = (nativeEventTarget) => (key) => (value) => () => nativeEventTarget[key] = value

// exports.getClassName = (nativeEventTarget) => nativeEventTarget["className"];

exports.getElementById = (id) => () => document.getElementById(id)

exports.setHTMLElementField = (key) => (value) => (elem) => () => elem[key] = value