## User interactions we want to support
- Block interactions
  - Create new term definition within a block
  - delete a term definition within a block
  - create datatype definition within a block
  - delete datatype definition within a block
  - cut/copy a term definition
  - paste a term definition
    - in the same block
    - in a different block
  - move a datatype definition
  - buffer interaction
- Declaration interactions
  - dig an argument
  - dig output
  - dig type
  - fill a type hole (which will always be in a declaration, thats where the types are)
- Base Type interactions
  - dig
  - fill a type hole
- Arrow Type interactions
  - do a Type List interaction in input
  - do a Base Type interaction on output
- Buffer interactions
    - cut/copy a neutral form from buffer
    - paste neutral form into buffer
- Neutral form interactions
  - dig (Get turned into HoleTerm)
  - cut/copy a neutral form
  - paste a neutral form into a hole (HoleTerm changes to what was pasted)
- Datatypes interactions
  - Create a new constructor
  - Delete a constructor
  - Drag constructors to reorder
  - Perform a Type List interaction on a constructor
- Type List interactions (input to an Arrow, or a constructor)
  - insert an argument
  - delete an argument
  - dig an argument
  - fill a type hole in an argument
  - click and drag an argument to a different position

# Unanswered Questions
- How can interactions which involve two locations, such as a "copy/paste" or a "click and drag" be handled?
- Should pasting into a hole be "at the hole" or "at the parent of the hole"? Seems simpler to do at the hole.
- In click and drag (even if its not in version 1), suppose the user drags over hole where the term fits. Then,
    it should show what the program would look like if the term was placed there (including type holes which
    may have been substituted elsewhere). But then, when the user drags off the hole, it should go back to how it was.
    How can these temporary states be represented? They must involve the model and not merely the rendering code,
    because they involve computing hole substitutions potentially.
    (unless we simplify and don't show that.)