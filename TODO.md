# TODO

Potential Plan:
- Replace block with single let
- Figure out how to move lets
- For consistency, the way you move something before a term is by dragging it onto the term. This is consistent with curried types and other stuff.
- Maybe a buffer is a let and maybe its something similar
- Buffers are displayed inline

- Jacob

  - [x] decide how cases work in match
  - [ ] figure out how to deal with having Changes and TypeChange at the same time.
  - [ ] function to move term from one context to another for copy/paste/drag
  - [ ] chAt*
    - [x] chAtModule
    - [ ] chAtTerm
    - [x] chAtCase
    - [x] chAtType
    - [ ] ...
  - [x] change\*
    - [x] changeTerm
    - [x] changeType
    - [x] changeDef
    - [x] changeBlock
    - [x] changeNeu
  - [x] changes syntax (curried)
  - [x] hole stuff (i.e. unification)

- Henry

  - next
    - drag a term "to the side" to pop it out into the nearest block (i.e. allowed scope)
    - drag a term onto a block handle to add it to that block anonymously
    - drag a term into a term hole
    - drag a type into a type hole
    - when in a term/type hole, can start typing and will present a list of names, which are filtered by the text input as well as compatible types

  - [ ] syntax
    - [x] upgrade lists to have metadata for each item
  - [ ] recursors
    - [x] base
    - [x] context + type
    - [x] metacontext (for derived metadata stuff: names, shadows, constructor
          ids, etc)
    - [x] index (still have to handle typechanges)
    - [x] transformation
  - [ ] index
    - [x] upgrade syntax/metadata so that lists have an `*ItemMetadata` for each
          element of the list
    - [x] upgrade index so that each step has constant number of children (lists
          have 2 children: head, tail)
    - [x] upgrade index recursor layer
    - [x] upgrade renderer
  - [ ] rendering
    - [x] basic outline
    - [x] indentation
    - [x] shadowing
      - [x] bug: paramter names in types not shadowing correctly, but lambdas
            do??
    - [ ] environment
      - context
      - goal type (if at a term)
      - available actions
    - [ ] actions
      - [x] hovering over syntax will highlight the innermost syntax element
            (even if hovering over the whitespace between)
      - [x] list manipulations
        - insertions: triggers is between list items
        - deletions: trigger is at list item
        - move: delete; insert
        - instances
          - [ ] definitionItems
          - [ ] constructorItems (in data definition)
          - [ ] parameterItems (in constructor)
          - argItems (in neutral), actually can't directly manipulated because
            must correspond to expected type
          - termIds (in case), can't be directly manipulated since must
            correspond to parameterItems of constructor
      - [ ] term holes
        - [x] show type in hole
        - [ ] drag a var from lambda/case/termBinding into hole (bug with
              highlighting)
        - [ ] drag a term into hole
        - paste into hole
        - basic fills:
          - neutral form (given a var in context)
            - checks all partial applications, and selects first viable
          - match (given term to match on)
          - lambda
        - perform type change on type in hole
      - [ ] lambda term
        - remove lambda around via RemoveArg typechange
      - [ ] term
        - dig
        - copy to clipboard
      - [x] renaming
        - things that can be renamed: parameters, termBindings, typeBindings
        - [x] type while selected
        - [ ] edits in place via `<input>` or changing moves and handling
              keyboard input
      - [ ] type changes
        - [x] enArrow
        - [x] delete (parameter)
        - [x] dig
        - [ ] swap
          - does an animation dragging the term you are swapping
    - [ ] cursor (worry about this after mouse actions work)
    - [ ] edit name of variable by lambda/case
      - problem: in the index recursor, how do I keep track of which type's
        parameters map to which Ids??
      - solution: remove name from parameters, instead is always tracked by each lambda-
