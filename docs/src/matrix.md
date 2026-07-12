---
title: Matrices
---

<img src="logo_bw.svg" alt="" height="80" style="float:right;margin:0 0 8px 16px"/>

# Matrices
<div style="clear:both"></div>

```scala mdoc:silent
import it.grypho.scala.leonardo.core.*
import it.grypho.scala.leonardo.scalar.*
import it.grypho.scala.leonardo.matrix.*
import it.grypho.scala.leonardo.parser.Parser

val env = new Environment()
```

## Constructing a matrix

`_Matrix.ofRows` accepts row vectors as `Vector[_Expression]` varargs:

```scala mdoc:silent
// 2×2 identity matrix
val I2 = _Matrix.ofRows(
  Vector(_Number(1.0), _Number(0.0)),
  Vector(_Number(0.0), _Number(1.0))
)
```

```scala mdoc
I2.toString
```

The parser accepts the same structure as a `[[…], […]]` literal:

```scala mdoc
Parser.parse("[[1, 0], [0, 1]]").get.toString
```

A row vector uses double brackets: `[[1, 2, 3]]`.

## Element types

Matrix elements can be arbitrary expressions — variables, functions, even
functionals. The matrix is *symbolic* until every element reduces to a number:

```scala mdoc:silent
val x = _Variable("x")
val symM = _Matrix.ofRows(
  Vector(x,                        Sin(x)),
  Vector(Power(x, _Number(2.0)),   _Number(1.0))
)
```

```scala mdoc
symM.toString
```

Evaluate with `x = 1`:

```scala mdoc
val envX = new Environment(5, Map("x" -> _Number(1.0)))
symM.eval(envX)
```

## Arithmetic

Standard `+`, `*`, scalar multiplication and transpose work on both symbolic
and concrete matrices:

```scala mdoc:silent
val A = Parser.parse("[[1, 2], [3, 4]]").get
val B = Parser.parse("[[5, 6], [7, 8]]").get
```

```scala mdoc
// A + B
MatSum(A, B).eval(env)
```

```scala mdoc
// A * B
MatProduct(A, B).eval(env)
```

```scala mdoc
// 2 * A
MatScale(_Number(2.0), A).eval(env)
```

```scala mdoc
// Aᵀ
Transpose(A).eval(env)
```

## Dense evaluation

When all elements reduce to numbers the result is a `_MatrixValue` — a dense
row-major `Array[Double]`. The multiply kernel is block-tiled for cache
efficiency and runs in parallel above a 2¹⁶-element work threshold:

```scala mdoc
MatProduct(A, B).eval(env) match
  case Right(m: _MatrixValue) => m.toVector
  case other                  => other
```

## Calculus on matrices

`derive`, `simplify`, and `expand` distribute element-wise over `_Matrix`
nodes (the `_ElementWise` marker enables this without domain-specific cases):

```scala mdoc:silent
val exprM = _Matrix.ofRows(
  Vector(Power(x, _Number(2.0)),  Sin(x)),
  Vector(Exp(x),                  _Number(1.0))
)
```

```scala mdoc
derive(exprM, x).toString
```

Matrix products do **not** distribute automatically under differentiation —
they need the product rule applied explicitly.
