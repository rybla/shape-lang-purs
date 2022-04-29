"use strict"

exports.eventKey = (event) => {
  // figure out how to get Shift, Control, etc modifiers
  console.log("event:"); 
  console.log(event);
  return event.key;
}