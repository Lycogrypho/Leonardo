---
title: Matrices
nav_order: 6
---

<img src="logo_bw.svg" alt="" style="height:80px;width:auto;float:right;margin:0 0 8px 16px"/>

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

Standard `+`, `*`, scalar multiplication, transpose, determinant and inverse
work on both symbolic and concrete matrices:

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

```scala mdoc
// det(A) — a scalar result (LU decomposition with partial pivoting on dense values)
Determinant(A).eval(env)
```

```scala mdoc
// inv(A) — the inverse matrix (Gauss–Jordan elimination); 1/A and M/N parse to this too
Inverse(A).eval(env)
```

`det(A)` reduces to a scalar `_Number`; a non-square or (for the inverse) singular
matrix has no result and stays symbolic, the same `x/0` contract used elsewhere.
The reciprocal spellings `1 / A` and `M / N` (= `M · N⁻¹`) route through the same
`Inverse` node. Small symbolic matrices expand by
[cofactors](https://en.wikipedia.org/wiki/Laplace_expansion) (determinant) and
[adjugate](https://en.wikipedia.org/wiki/Adjugate_matrix)/det (inverse). Algorithm
references:
[LU decomposition](https://en.wikipedia.org/wiki/LU_decomposition),
[Gauss–Jordan elimination](https://en.wikipedia.org/wiki/Gaussian_elimination#Gauss%E2%80%93Jordan_elimination).

## Dense evaluation

When all elements reduce to numbers the result is a `_MatrixValue` — a dense
row-major `Array[Double]`. The multiply kernel is block-tiled for cache
efficiency and runs in parallel above a 2¹⁶-element work threshold:

```scala mdoc
MatProduct(A, B).eval(env) match {
  case Right(m: _MatrixValue) => m.toVector
  case other                  => other
}
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

## Functions on matrices

Scalar functions (`sin`, `exp`, `ln`, …) applied to a matrix distribute
element-wise. This works for a dense value and for a symbolic matrix alike —
numeric cells fold, free-variable cells stay as `f(cell)` until bound:

```scala mdoc
// exp over a symbolic matrix: exp(x) stays symbolic, exp(0) folds to 1.0
Exp(_Matrix.ofRows(Vector(x, _Number(0.0)))).eval(env) match {
  case Left(m)  => m.toString
  case Right(v) => v.toString
}
```

Binding the free variables lets the whole matrix collapse to a dense value:

```scala mdoc
Exp(_Matrix.ofRows(Vector(x, _Number(0.0)))).eval(envX) match {
  case Right(v) => v.toString
  case Left(m)  => m.toString
}
```

## Decompositions

Each decomposition returns its factors bundled as a 1×n row of matrices, so one indexing
mechanism (`at(result, 1, k)`, or tuple assignment in the REPL) serves them all:

| Call | Result | Algorithm |
|---|---|---|
| `lu(A)` | `[[L, U, P]]`, `P·A = L·U` | [LU decomposition](https://en.wikipedia.org/wiki/LU_decomposition) with partial pivoting |
| `qr(A)` | `[[Q, R]]`, `A = Q·R` | [QR decomposition](https://en.wikipedia.org/wiki/QR_decomposition) by modified [Gram–Schmidt](https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process) |
| `eigen(A)` | `[[λ₁, …, λₙ]]` | [QR algorithm](https://en.wikipedia.org/wiki/QR_algorithm) with Wilkinson shifts; complex pairs come back as `_Complex` |
| `eig(A)` | `[[V, D]]`, `A·V = V·D` | [Eigendecomposition](https://en.wikipedia.org/wiki/Eigendecomposition_of_a_matrix): eigenvector columns and the diagonal eigenvalue matrix |
| `jordan(A)` | `[[P, J]]`, `A = P·J·P⁻¹` | [Jordan normal form](https://en.wikipedia.org/wiki/Jordan_normal_form) for diagonalizable input; a defective matrix stays symbolic |

```scala mdoc
Parser.parse("eigen([[2, 1], [1, 2]])").get.eval(env).toExpression.toString
```

```scala mdoc
Parser.parse("lu([[4, 3], [6, 3]])").get.eval(env).toExpression.toString
```

## Matrix exponential

`expm(A)` computes `e^A` — the
[matrix exponential](https://en.wikipedia.org/wiki/Matrix_exponential), a different
operation from the integer power `A^n`. It uses
[scaling and squaring](https://en.wikipedia.org/wiki/Matrix_exponential#Computing_the_matrix_exponential)
with a degree-13 Padé approximant, which is why it also works for **defective** matrices —
input the eigendecomposition route has no basis for:

```scala mdoc
// nilpotent: the series terminates, so the result is exact
Parser.parse("expm([[0, 1], [0, 0]])").get.eval(env).toExpression.toString
```

```scala mdoc
// a rotation generator exponentiates to the rotation matrix
Parser.parse("expm([[0, -1], [1, 0]])").get.eval(env).toExpression.toString
```
