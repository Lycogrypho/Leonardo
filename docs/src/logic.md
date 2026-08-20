---
title: Logic
nav_order: 7
---

<img src="logo_bw.svg" alt="" height="80" style="float:right;margin:0 0 8px 16px"/>

# Logic
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

The `logic` package provides the five connectives — `And`, `Or`, `Not`,
`Implies`, `Xor` — as ordinary expression nodes over the shared AST. Operands are
untyped `_Expression`s, so equations and any other domain compose freely. `eval`
runs the Kleene/Zadeh min–max rule table (`and` = min, `or` = max, `not` = 1 − a)
and reduces to a `_Bool` when the operands do:

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

## Three-valued (Kleene) logic

The same connectives and the same rule table carry a third truth value,
`unknown` — the value carrier widens, the operators do not. `unknown` is
`_Truth.Unknown`, a first-class value (bindable, printable, saved by `:save`)
sitting at degree `0.5`:

```scala mdoc
Parser.parse("unknown").get
Not(_Truth.Unknown).eval(env)                       // 0.5 is the negation fixpoint
And(_Truth.Unknown, _Truth.Unknown).eval(env)
```

The crisp cases still decide, because `0` annihilates `min` and `1` annihilates
`max` — which is exactly why `And`/`Or`'s short-circuits stay valid unchanged:

```scala mdoc
And(_Bool(false), _Truth.Unknown).eval(env)         // false and unknown = false
Or(_Bool(true), _Truth.Unknown).eval(env)           // true or unknown = true
And(_Bool(true), _Truth.Unknown).eval(env)          // true and unknown = unknown
```

`_Truth.of` collapses the crisp endpoints back to `_Bool`, so every boolean
pattern match keeps firing and the classical truth tables come out of the graded
table unchanged — the boolean logic above is a special case, not a separate path:

```scala mdoc
_Truth.of(1.0)
_Truth.of(0.0)
And(_Bool(true), _Bool(true)).eval(env)             // a _Bool, not a _Truth
```

`kleeneTable` is the three-valued counterpart of `truthTable`, enumerating `3^n`
rows (up to 10 variables); the REPL spells it `truth3`:

```scala mdoc
kleeneTable(Not(a), List(a)).map((row, result) => (row("a").toString, result.map(_.toString)))
```

```
leonardo> truth3 a and not a
a       | (a and (not a))
false   | false
unknown | unknown
true    | false
```

That last row is the point: `unknown and not unknown` is `unknown`, not `false`.
The classical laws that fail here — complement in `and`/`or`, `a implies a`, and
`a xor a` — are gated on the expression being free of `_Truth` degrees, so
simplification never "proves" them away:

```scala mdoc
simplifyLogicFully(And(_Truth.Unknown, Not(_Truth.Unknown)))   // stays unknown
simplifyLogicFully(And(a, Not(a)))                             // free variables: crisp atoms
```

A free variable counts as a crisp atom — that is the documented domain
restriction of `simplifyLogic`, `toCNF`, and `toDNF`. If a variable may hold
`unknown`, bind it and use `eval` rather than `simplify`.

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
