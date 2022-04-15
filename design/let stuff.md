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

## Disadvantages