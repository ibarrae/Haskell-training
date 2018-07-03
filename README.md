# haskell-training

## WHNF (Weak Head Normal Form) vs. NF (Normal Form)
- WHNF stands for an expression that has not yet been fully evaluated.
- NF stands for an expression that has been fully evaluated.
- Between both of them, the main difference is that in NF can be simple expressions, functions or constructors, but in a case you add one extra operation to any of these, they transform into WHNF

## Lazy and Strict evaluation
- Lazy evaluation creates a thunk with the expression that needs evaluation and when it is actually needed, it calculates the expression.
- Strict evaluation allways evaluates a given expression on the go.
