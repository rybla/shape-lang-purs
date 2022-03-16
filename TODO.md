# TODO

- Jacob

  - [x] decide how cases work in match
  - [ ] recursion principle with wrap (uses TypeChanges context)
  - [ ] change\*
    - [x] changeTerm
    - [x] changeType
    - [ ] changeDef
    - [ ] changeBlock
    - [ ] changeNeu
  - [x] changes syntax (curried)
  - [x] hole stuff (i.e. unification)

- Henry
  - [ ] syntax
    - [x] upgrade lists to have metadata for each item
  - [ ] recursors
    - [x] base
    - [x] context + type
    - [x] metacontext (for derived metadata stuff: names, shadows, constructor
          ids, etc)
    - [ ] index (still have to handle typechanges)
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
    - [ ] actions
    - [ ] cursor (worry about this after mouse actions work)
