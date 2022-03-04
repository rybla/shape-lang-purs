## User interactions we want to support
- Block interactions
  - create/move/delete term definition within a block
  - create/move/delete data definition within a block
- Signature interactions
  - dig argument/output
  - add/move/delete argument
  - fill a type hole (which will always be in a declaration, thats where the types are)
- Term interactions
  - dig (Get turned into HoleTerm)
  - Hole
    - intro lambda (if hole's type is arrow)
    - paste/move term into hole
  - Application
    - add/delete argument
- Datatypes interactions
  - create/move/delete constructor

# Unanswered Questions
- [x] How can interactions which involve two locations, such as a "copy/paste" or a "click and drag" be handled?
- [x] Should pasting into a hole be "at the hole" or "at the parent of the hole"? Seems simpler to do at the hole.
- [x] In click and drag (even if its not in version 1), suppose the user drags over hole where the term fits. Then,
    it should show what the program would look like if the term was placed there (including type holes which
    may have been substituted elsewhere). But then, when the user drags off the hole, it should go back to how it was.
    How can these temporary states be represented? They must involve the model and not merely the rendering code,
    because they involve computing hole substitutions potentially.
    (unless we simplify and don't show that.)
