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

## Symmetric ternary: the same logic in different digits

Symmetric ternary spells the three truth values with the digits `{-1, 0, 1}`
instead of `{false, unknown, true}`. The two alphabets are related by the affine
map `t = (s + 1) / 2`, so this is an **encoding**, not a second semantics — the
rule table above is untouched:

```scala mdoc
_Truth.fromSymmetric(-1.0)
_Truth.fromSymmetric(0.0)
_Truth.fromSymmetric(1.0)
_Truth.toSymmetric(_Truth.Unknown)
```

The encoding is switched on per `Environment` (the REPL spells it
`logic symmetric on`). With it on, the digits are additionally *read* as truth
values in connective positions, and the Kleene identities restate verbatim:

```scala mdoc:silent
val sym = new Environment(Environment.DefaultPrecision, Map.empty, symmetricLogic = true)
```

```scala mdoc
Parser.parse("-1 and 0").get.eval(sym)     // false and unknown = false
Parser.parse("1 or 0").get.eval(sym)       // true or unknown = true
Parser.parse("0 and 0").get.eval(sym)      // unknown and unknown = unknown
Parser.parse("not 0").get.eval(sym)        // not unknown = unknown
```

The guard matters: with the encoding **off** a bare `0` is a plain number, so a
connective over it stays symbolic rather than silently becoming a truth value,
and ordinary arithmetic is never reinterpreted in either mode:

```scala mdoc
And(_Number(1.0), _Number(0.0)).eval(env)  // default: stays symbolic
Parser.parse("2 * 3 + 1").get.eval(sym)    // symmetric: still just arithmetic
```

Because the flag lives on the `Environment`, a *bound* variable participates too
— which a purely textual encoding could not do:

```scala mdoc
And(a, _Bool(true)).eval(sym.withBinding("a", _Number(0.0)))
```

One consequence worth noting: under this encoding the digit `0` is `unknown`, so
it counts as graded for the crisp-only gate — `0 and not 0` must not fold to
`false` by complement:

```scala mdoc
simplifyLogicFully(And(_Number(0.0), Not(_Number(0.0))), identity, symmetric = true)
```

At the REPL the toggle is persisted by `:save`, but the script always writes the
word spelling, so saved sessions stay portable across the setting:

```
leonardo> logic symmetric on
leonardo> truth3 not a
a  | (not a)
-1 | 1
0  | 0
1  | -1
```

## Fuzzy logic: the full [0, 1] interval

The `_Truth` carrier already holds any degree in `[0, 1]`, so the fuzzy tier needs no
new value type — only a way to *produce* degrees and a choice of how they combine.
`truth(x)` converts a scalar degree, and is also how a graded degree prints, so it
round-trips:

```scala mdoc
Parser.parse("truth(0.25)").get.eval(env)
_Truth.of(0.25).toString
And(_Truth.of(0.3), _Truth.of(0.7)).eval(env)     // min
Not(_Truth.of(0.3)).eval(env)                     // 1 - a
```

### Membership functions and hedges

Membership curves map a crisp measurement into `[0, 1]`, and the hedges reshape a
degree. They evaluate to truth values, so they compose with the connectives directly
— no cast at each step:

```scala mdoc
Parser.parse("trimf(2.5, 0, 5, 10)").get.eval(env)     // triangular, halfway up
Parser.parse("gaussmf(3, 3, 1)").get.eval(env)         // gaussian, at the mean
Parser.parse("very(0.5)").get.eval(env)                // concentration: d squared
Parser.parse("somewhat(0.25)").get.eval(env)           // dilation: sqrt d
Parser.parse("very(0.5) and somewhat(0.25)").get.eval(env)
```

The built-in set is `trimf(x, a, b, c)`, `trapmf(x, a, b, c, d)`,
`gaussmf(x, mean, sigma)`, `sigmf(x, a, c)`, plus the hedges `very` and `somewhat`.

### Custom membership functions

The built-in curves are a convenience, not a limit: `truth(x)` takes an **arbitrary
scalar expression**, so any function of one variable is a membership curve. Here is a
Cauchy bell, which is none of the four built-in shapes:

```scala mdoc:silent
val bell = Parser.parse("truth(1 / (1 + (x - 5)^2))").get
def at(v: Double) = bell.eval(env.withBinding("x", _Number(v)))
```

```scala mdoc
at(5.0)     // the centre
at(4.0)     // one unit out
at(0.0)
```

A custom curve is not second-class — it composes with the hedges, the connectives and
`defuzz` exactly like a built-in one:

```scala mdoc
Parser.parse("very(truth(1 / (1 + (x - 5)^2)))").get.eval(env.withBinding("x", _Number(4.0)))
centroid(bell, _Variable("x"), 0.0, 10.0)
```

At the REPL a curve is usually given a name first, and then used by that name:

```
leonardo> bell := truth(1 / (1 + (x - 5)^2))
leonardo> defuzz(bell, x, 0, 10)
5.0
```

Two rules worth knowing. First, a membership degree lives in `[0, 1]`: an argument
outside that interval is not a degree, so the node stays symbolic rather than
clamping — which is how a custom curve tells you it has left the unit interval.

```scala mdoc
Parser.parse("truth(2)").get.eval(env)     // stays symbolic, not clamped to true
```

Second, `defuzz` also accepts a bare scalar curve with no `truth(...)` wrapper, and
either way only points inside `[0, 1]` count as membership:

```scala mdoc
Parser.parse("defuzz(1 / (1 + (x - 5)^2), x, 0, 10)").get.eval(env)
```

### Aggregating curves

Because degrees are truth values, curves combine with the ordinary connectives — the
usual fuzzy-inference step, where each rule contributes a curve and the results are
unioned before defuzzifying:

```scala mdoc
Parser.parse("defuzz(trimf(x, 0, 3, 6) or trimf(x, 4, 7, 10), x, 0, 10)").get.eval(env)
```

The connectives are admitted in the membership positions specifically — the argument
of `defuzz`, `very`, and `somewhat`. Ordinary function arguments stay arithmetic, so
`sin(a and b)` is still a parse error.

### Alternative t-norms

Which t-norm combines degrees is an `Environment` parameter, not a separate package or
node: min–max (the default), product, or Łukasiewicz. All three agree with classical
logic on the crisp values, so the boolean and three-valued tiers are unaffected:

```scala mdoc:silent
val prod  = new Environment(Environment.DefaultPrecision, Map.empty, false, LogicSemantics.Product)
val lukas = new Environment(Environment.DefaultPrecision, Map.empty, false, LogicSemantics.Lukasiewicz)
```

```scala mdoc
And(_Truth.of(0.3), _Truth.of(0.5)).eval(prod)    // 0.3 * 0.5
Or(_Truth.of(0.3), _Truth.of(0.5)).eval(prod)     // a + b - a*b
And(_Truth.of(0.8), _Truth.of(0.7)).eval(lukas)   // max(0, 1.5 - 1)
And(_Bool(true), _Bool(true)).eval(lukas)         // crisp: unchanged
```

Only min–max is a *lattice*, so idempotence and absorption hold for graded degrees
there alone — `a and a` is `a` squared under the product t-norm. Simplification gates
those two rules on the semantics accordingly, on top of the crisp-only gate above:

```scala mdoc
simplifyLogicFully(And(_Truth.of(0.3), _Truth.of(0.3)), identity, false, LogicSemantics.MinMax)
simplifyLogicFully(And(_Truth.of(0.3), _Truth.of(0.3)), identity, false, LogicSemantics.Product)
```

### Defuzzification

Defuzzifying collapses a membership curve back to one crisp value. `centroid` (the
centre of gravity) is what `defuzz(e, v, lo, hi)` uses in the grammar; `meanOfMaxima`
and `bisector` are available as library functions:

```scala mdoc
val tri = Parser.parse("trimf(x, 0, 5, 10)").get
centroid(tri, _Variable("x"), 0.0, 10.0)
meanOfMaxima(tri, _Variable("x"), 0.0, 10.0)
Parser.parse("defuzz(trimf(x, 0, 5, 10), x, 0, 10)").get.eval(env)
```

Sampling takes `scalar.compile`'s fast path when the curve is pure scalar arithmetic
and falls back to tree evaluation for membership and connective nodes — this is where
`logic` gains its `scalar` import, the same relationship `matrix` already has.

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
