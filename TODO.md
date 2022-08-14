# TODO

- [ ] for the variable query mode drop-down menu, add mouse hover and mouse
  click interaction
- [x] escape key to deselect program
- [ ] filter variable query results by `fitsInHole`, so probably need
  `VariableQueryMode` to have two lists, one which is the unfiltered list of items
  from context, along with their `fitsInHole` status, and one which is the
  filtered list via the query
- [ ] make variable query mode put the query be in the hole's data as an actual
  attribute, so that its not part of the global mode that can be moved between
  holes which isn't right 