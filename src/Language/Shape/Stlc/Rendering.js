"use strict"

exports.code = function(e) {return e.code}

exports.setNativeEventTargetProp = (nativeEventTarget) => (key) => (value) => () => nativeEventTarget[key] = value