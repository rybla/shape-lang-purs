# Let stuff

## Proposal

Currently, part our syntax looks like this:

```
<block> ::= <def*> in <term>

<term> ::= fun <name> => <block>
         | <name> <term*>
         | <hole>
         | match <term> : <type> with <case*>

<case> ::= <constr> <var*> => <block>
```

The proposal is to change this part of the syntax to something like this:

```
<block> ::= <def*> in <term>

<term> ::= fun <name> => <term>
         | <name> <term*>
         | <hole>
         | match <term> : <type> with <case*>
         // which one of these??
         | <def> in <term>
         | <def*> in <term>

<case> ::= <constr> <var*> => <term>
```

The idea is that a block is now just a let, which is a term. And if the let only has ony definition, then we can decide whether or now to allow mutually recursive definitions.

## Advantages

- Would make it easier to explain the grammar to people
- Potentially could be confusing to a user where they can insert definitions
- Potentially could make displacing terms simpler, although one could debate the desired behaviour here
- We already have situations where "cursor on the left of e" is equivalent to "select e". So by analogy, we are identifying "put a definition before e" with "wrap e with a let"

## Disadvantages

- Makes mutual recursion a question. Do we have both nested lets and let* for recursion? If so, what is the user interface for dragging definitions to different places in the program?
- If the top level program uses lets, then this would allow you to select all definitions after a given definition, which seems wierd. Could solve with let* or separate concept of top level definition
- One feature that we want is to be able to drag a term from your program into a "scratch area", which is just a let bound definition. What would this interface look like? Would you trag it onto another term and then it goes into a let around that term? Should we have a different concept of scratch area that isn't just a let?
- My biggest concern with either approach is how we can make the process of plugging blocks of code together as intuitive as it is in scratch
