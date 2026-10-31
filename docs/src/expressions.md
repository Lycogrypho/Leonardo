---
title: Expressions & Evaluation
nav_order: 4
---

<img src="logo_bw.svg" alt="" style="height:80px;width:auto;float:right;margin:0 0 8px 16px"/>

# Expressions & Evaluation
<div style="clear:both"></div>

```scala mdoc:silent
import it.grypho.scala.leonardo.core.*
import it.grypho.scala.leonardo.scalar.*
```

## The expression hierarchy

Every node in the AST extends `_Expression`. The main families are:

| Type | Examples |
|------|---------|
| **Values** (`_Value`) | `_Number(d)`, `_Complex(re, im)`, `_Rational`, `_MatrixValue`, `_Bool`, `_Truth`, `_Interval` |
| **Atoms** | `_Variable("x")` |
| **Operations** | `Sum`, `Product`, `Ratio`, `Power` |
| **Functions** | `Sin`, `Cos`, `Tg`, `Exp`, `Ln`, `LogBase`, `Asin`, `Acos`, `Atan`, plus the hyperbolic and reciprocal families |
| **Functionals** | `_Derivative`, `_Integral`, `_DefIntegral`, `_Limit`, `_Taylor`, `_Laplace`, … |

All nodes are immutable; the tree is value-typed and safe to share across threads.

## The dual eval model

Every `_Expression` implements:

```scala
def eval(env: Environment): Either[_Expression, _Value]
```

`Right[_Value]` means the expression reduced to a concrete result.
`Left[_Expression]` means it stayed symbolic: variables remain free, or the operation is
undefined at the point — `1/0` and `ln(0)` stay symbolic rather than throwing or producing
an infinity.

```scala mdoc:silent
val x = _Variable("x")
val y = _Variable("y")
val env = new Environment()
```

A plain variable without a binding stays left (symbolic):

```scala mdoc
x.eval(env)
```

A constant folds immediately to right:

```scala mdoc
_Number(42.0).eval(env)
```

A compound expression reduces as far as the bindings allow:

```scala mdoc
val expr = Sum(Product(_Number(2.0), x), y)
// 2x + y — both free, stays symbolic
expr.eval(env)
```

Bind `x` only — `y` remains free:

```scala mdoc
val envX = new Environment(5, Map("x" -> _Number(3.0)))
expr.eval(envX)
```

Bind both — reduces to a number:

```scala mdoc
val envXY = new Environment(5, Map("x" -> _Number(3.0), "y" -> _Number(1.0)))
expr.eval(envXY)
```

## Free variables

Every node caches its free variable set after the first traversal (`O(1)` thereafter):

```scala mdoc
expr.freeVars
```

```scala mdoc
_Number(1.0).freeVars
```

## Simplification

`simplify` performs a single structural pass; `simplifyFully` repeats until
the tree stops changing:

```scala mdoc
simplify(Sum(x, _Number(0.0))).toString
```

```scala mdoc
simplify(Product(_Number(1.0), x)).toString
```

```scala mdoc
// Constant folding
simplify(Sum(_Number(3.0), _Number(4.0))).toString
```

```scala mdoc
// Inverse function pairs
simplify(Ln(Exp(x))).toString
```

## Expansion

`expand` distributes multiplication over addition, expands integer powers of sums up to the
20th, and collects like terms as it goes, so `(a + b)^20` is 21 terms. An answer that is a
polynomial in **one** variable is written by descending degree, the textbook form; with several
variables the terms keep the order they first appear in. A sum over a numeric divisor is divided
term by term (`(4x + 6y)/2` expands to `2x + 3y`). A product whose expansion would be deeper than the library's limit of 200
levels (a sum is as deep as it has terms) is left as written rather than built; at the REPL a
note says so:

```scala mdoc
val a = _Variable("a")
val b = _Variable("b")
expand(Power(Sum(a, b), _Number(2.0))).toString
```

```scala mdoc
expand(Product(Sum(a, _Number(1.0)), Sum(b, _Number(2.0)))).toString
```

## Environment and precision

`Environment` is immutable. `withBinding` returns a new instance — the original
is unchanged, making concurrent evaluation safe:

```scala mdoc:silent
val base = new Environment(5, Map("x" -> _Number(1.0)))
val withY = base.withBinding("y", _Number(2.0))
```

```scala mdoc
base.isBound("y")
```

```scala mdoc
withY.isBound("y")
```

The `precision` field controls rounding at display time only — no precision is
lost during intermediate computation:

```scala mdoc
_Number(math.Pi).display(3)
```

```scala mdoc
_Number(math.Pi).display(10)
```

## Intervals

An interval `[lo, hi]` is a value whose endpoints are **rigorous bounds**: every operation
returns an interval guaranteed to contain every value the operation can take on its operands.
Each endpoint is rounded *outward* — down at the bottom, up at the top — so the answer holds
whatever rounding the arithmetic did on the way. It answers a question the exact tier does not:
not "what is this number exactly" but "what can this result be, given ranges for its inputs".

```scala mdoc:silent
import it.grypho.scala.leonardo.parser.Parser
def interval(s: String) = Parser.parse(s).get.eval(new Environment())
```

```scala mdoc
interval("[1, 2] * [-1, 3]")
interval("[-2, 1]^2")
interval("sin([0, pi])")
```

The last two show the parts that are easy to get wrong. An even power through zero starts at
`0`, not at `1`, and `sin` reaches its maximum *inside* `[0, π]`, so the range is `[0, 1]`,
not the values at the two ends. Its lower bound is exactly `0`, the value at `0`, which is
known exactly and so is not widened; at the other end `pi` is the nearest `Double`, whose sine
is a positive `10⁻¹⁶`.

An interval never learns that two of its occurrences are the same quantity, so `x - x` over
`[1, 2]` is `[−1, 1]`, not `0`. That is correct interval arithmetic, not a defect: each
occurrence ranges over the interval independently.

```scala mdoc
interval("[1, 2] - [1, 2]")
```

What stays symbolic, by design:
- a divisor containing zero, whose quotient is two rays rather than an interval;
- a function outside its domain, such as `ln([−1, 1])`, or `tan` across a pole;
- a function with no interval rule. The covered ones are `exp`, `ln`, `log`, `sin`, `cos`,
  `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `abs` and powers.

Comparisons answer `true` or `false` only when the relation holds for every pair of points, and
`unknown` otherwise: `[1, 3] < [2, 4]` is `unknown`. Beside a value that has no real bounds — a
complex number, a matrix, a truth value — a relation is not a question about points at all, so
`[1, 2] = 2i` stays as written rather than being answered `unknown`.

Interval endpoints written in exact mode are exact rationals, combined exactly and bounded
through arbitrary-precision arithmetic where an operation leaves the rationals. An interval is
a result, never an operand of symbolic algebra: `simplify` and `expand` do not reason about one,
and it never reads as a single number anywhere a number is expected.
