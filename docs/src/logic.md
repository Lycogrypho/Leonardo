---
title: Boolean Logic
nav_order: 7
---

<img src="logo_bw.svg" alt="" height="80" style="float:right;margin:0 0 8px 16px"/>

# Boolean Logic
<div style="clear:both"></div>

```scala mdoc:silent
import it.grypho.scala.leonardo.core.*
import it.grypho.scala.leonardo.logic.*
import it.grypho.scala.leonardo.parser.Parser

val a = _Variable("a")
val b = _Variable("b")
val env = new Environment()
```

## Connectives

The `logic` package provides the five boolean connectives — `And`, `Or`, `Not`,
`Implies`, `Xor` — as ordinary expression nodes over the shared AST. Operands are
untyped `_Expression`s, so equations and any other domain compose freely. `eval`
reduces to a `_Bool` when the operands do:

```scala mdoc
And(_Bool(true), _Bool(false)).eval(env)
```

In the grammar the connectives are the word operators `and`, `or`, `not`,
`implies`, `xor`, with `true`/`false` as literals. They bind **looser** than
`=`/`==`, with precedence (tightest to loosest) `not` > `and` > `xor` > `or` >
`implies` (`implies` is right-associative):

```scala mdoc
Parser.parse("a or b and c").get
Parser.parse("x = 1 and y = 2").get
Parser.parse("true and false").get.eval(env)
```

A connective over an unbound variable stays symbolic with the operands reduced,
following the library-wide dual-evaluation contract:

```scala mdoc
And(a, _Bool(true)).eval(env)
And(a, _Bool(true)).eval(env.withBinding("a", _Bool(true)))
```

`And`, `Or`, and `Implies` short-circuit exactly as `Product.eval` does on a zero
operand: `false and X` is `false` — and `true or X` is `true` — without evaluating
`X`, even when `X` could not reduce at all:

```scala mdoc
// 1/0 = 0 cannot reduce, but the left operand decides
And(_Bool(false), Parser.parse("1/0 = 0").get).eval(env)
```

## Simplification

`simplifyLogic` (and its fixpoint `simplifyLogicFully`) applies constant folding,
double negation, idempotence, complement, and absorption:

```scala mdoc
simplifyLogicFully(And(a, _Bool(true)))
simplifyLogicFully(Not(Not(a)))
simplifyLogicFully(Or(a, And(a, b)))
simplifyLogicFully(And(a, Not(a)))
```

The REPL's `simplify` command runs this pass after the scalar pass, injecting
`scalar.simplifyFully` for scalar bodies nested inside connectives.

## Normal forms

`toCNF` / `toDNF` rewrite an expression into conjunctive / disjunctive normal
form: `implies`/`xor` are desugared, `not` is pushed to the leaves via De Morgan,
and the appropriate connective is distributed. A distribution that would exceed
1024 clauses returns the input unchanged:

```scala mdoc
toCNF(Not(And(a, b)))
toCNF(Implies(a, b))
toDNF(And(a, Or(b, _Variable("c"))))
toCNF(Or(a, Not(a)))
```

These algorithms are crisp-only: they treat non-connective sub-expressions as
opaque atoms and are valid for boolean operands only.

## Truth tables

`truthTable` enumerates every boolean assignment of a variable list (up to 16
variables) and evaluates the expression under each; rows that cannot reduce to a
boolean yield `None`:

```scala mdoc
truthTable(Xor(a, b), List(a, b))
```

The REPL command `truth <expr>` prints the same table over the expression's free
variables:

```
leonardo> truth a and b
a     b     | (a and b)
false false | false
false true  | false
true  false | false
true  true  | true
```
