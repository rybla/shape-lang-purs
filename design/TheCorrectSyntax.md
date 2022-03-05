# The Correct Syntax

`LambdaTerm` and `ArrowType` are curried, but `AppTerm` is not. This is because
currying for `LambdaTerm` and `ArrowType` correspond -- both to the right. But,
application curries in the opposite direction -- to the left.

```haskell
data Module = Module [Definition]
data Block = Block [Definition] Term
data Definition
    = TermDefinition Name Type Term
    | DataDefinition Name [(Name, Type)]
data Term
    = LambdaTerm Name Type Block
    | AppTerm Name [Term]
    | HoleTerm
data Type
    = ArrowType Type Type
    | DataType Name
    | HoleType HoleID TypeWeakening
```
