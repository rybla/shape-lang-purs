# Changes

CPS changes:

```
P ::= B P P | L
PM ::= B PM PM [() -> P] | L

coCh : (P -> P) -> P -> PM
coCh wrap (B P1 P2) =
  B (coCh (lam x . wrap (B x P2)))
    (coCh (lam x . wrap (B P1 x)))
    [lam _ . wrap L, lam _ . wrap (B P2 P1)]
coCh wrap L = L
```
