"use strict"

exports.getElementById = id => () => document.getElementById(id)

exports.setHighlight = b => e => () => {
    if (b)
      e.classList.add("highlighted")
    else 
      e.classList.remove("highlighted")
  }
