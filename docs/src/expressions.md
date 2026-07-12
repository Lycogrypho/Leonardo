---
title: Expressions & Evaluation
---

<img src="logo_bw.svg" alt="" height="80" style="float:right;margin:0 0 8px 16px"/>

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
| **Values** (`_Value`) | `_Number(d)`, `_Complex(re, im)`, `_MatrixValue`, `_Bool` |
| **Atoms** | `_Variable("x")` |
| **Operations** | `Sum`, `Product`, `Ratio`, `Power` |
| **Functions** | `Sin`, `Cos`, `Tg`, `Exp`, `Log`, `Asin`, `Acos`, `Atan` |
| **Functionals** | `_Derivative`, `_Integral`, `_DefIntegral` |

All nodes are immutable; the tree is value-typed and safe to share across threads.

## The dual eval model

Every `_Expression` implements:

```scala
def eval(env: Environment): Either[_Expression, _Value]
```

`Right[_Value]` means the expression reduced to a concrete result.
`Left[_Expression]` means it stayed symbolic — one or more variables are still free.

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
simplify(Log(Exp(x))).toString
```

## Expansion

`expand` distributes multiplication over addition and expands integer powers of sums:

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
