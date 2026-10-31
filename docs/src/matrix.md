---
title: Matrices
nav_order: 6
---

<img src="logo_bw.svg" alt="" style="height:80px;width:auto;float:right;margin:0 0 8px 16px"/>

# Matrices
<div style="clear:both"></div>

*Calling this from Scala? **[L11 — Matrices](lessons/l11.md)** shows the API's shape — the two
carriers, the parser's structural dispatch, and decompositions that answer a row. This page is
the reference.*

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

## Products, trace and norms

`dot(u, v)` is the number `Σ uᵢvᵢ` of two rows or columns of one length, in any mix — where
`transpose(u) * v` is a `1×1` matrix, which does not add to a number. It takes **no
conjugate**, so a complex vector stays unevaluated rather than meeting one convention or the
other. `cross(u, v)` is the **right-handed** product of two 3-vectors, in the first one's shape:

```scala mdoc
Parser.parse("dot([[1], [2], [3]], [[4], [5], [6]])").get.eval(env)
Parser.parse("cross([[1], [0], [0]], [[0], [1], [0]])").get.eval(env)
```

`trace(A)` sums a square matrix's diagonal, and equals the sum of its eigenvalues:

```scala mdoc
Parser.parse("trace([[2, 1], [1, 3]])").get.eval(env)
```

`size(A)` is the row `[[rows, cols]]`, so `r, c := size(A)` at the prompt names both, and
`size(A, 1)` / `size(A, 2)` give one dimension; a number is a `1×1` matrix:

```scala mdoc
Parser.parse("size([[1, 2, 3], [4, 5, 6]])").get.eval(env)
Parser.parse("size([[1, 2, 3], [4, 5, 6]], 2)").get.eval(env)
```

`norm(x)` and `norm(x, kind)` read **the operand's shape** (MATLAB's rule): a row or a column is
a vector, anything else a matrix. The default is the 2-norm either way — for a matrix the
largest singular value, which the vector 2-norm equals on a column — and `kind` is `1`, `2`,
`inf` or `fro`, or for a vector any `p ≥ 1`. On a vector `1` and `inf` are the sum and the largest
modulus; on a matrix the largest column and row sums:

```scala mdoc
Parser.parse("norm([[3], [4]])").get.eval(env)
Parser.parse("norm([[1, -2], [-3, 4]], 1)").get.eval(env)
Parser.parse("norm([[1, -2], [-3, 4]], inf)").get.eval(env)
Parser.parse("norm([[1, -2], [-3, 4]])").get.eval(env)
```

All four stay exact on exact cells, surds of one radical included: `norm([[2], [3]])` is `√13`
and `norm([[√2], [1]], 1)` is `1 + √2`; a 2-norm whose sum of squares is not rational takes
its root at the working precision, and the matrix 2-norm demotes, as `svd` does.  Symbolic
cells give a symbolic answer, except the max-norm and a matrix's operator norms, which stay
unevaluated. A `Double` 2-norm is scaled by
its largest entry before squaring, so `norm([[1e200], [1e200]])` is not an overflow.

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
| `lu(A)` | `[[L, U, P]]`, `P·A = L·U` | [LU decomposition](https://en.wikipedia.org/wiki/LU_decomposition) with partial pivoting; over the rationals for an exact `A`, where a singular matrix factors too (a zero on `U`'s diagonal) |
| `qr(A)` | `[[Q, R]]`, `A = Q·R` | [QR decomposition](https://en.wikipedia.org/wiki/QR_decomposition) by modified [Gram–Schmidt](https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process); dependent columns stay unevaluated, since that refusal is the rank test `regress` relies on |
| `eigen(A)` | `[[λ₁, …, λₙ]]` | [QR algorithm](https://en.wikipedia.org/wiki/QR_algorithm) with Wilkinson shifts; complex pairs come back as `_Complex`; sorted by real part, then imaginary part, as `tfpoles` sorts its roots |
| `eig(A)` | `[[V, D]]`, `A·V = V·D` | [Eigendecomposition](https://en.wikipedia.org/wiki/Eigendecomposition_of_a_matrix): eigenvector columns and the diagonal eigenvalue matrix; a repeated eigenvalue gets independent eigenvectors when its eigenspace allows, and a defective matrix stays symbolic |
| `jordan(A)` | `[[P, J]]`, `A = P·J·P⁻¹` | [Jordan normal form](https://en.wikipedia.org/wiki/Jordan_normal_form): exact, Jordan blocks included, for an exact matrix whose eigenvalues are all rational (eigenvalues ascending, blocks by decreasing size, the `1`s above the diagonal); otherwise the diagonal case, and a defective numeric matrix stays symbolic, since a numeric Jordan block is not computable |

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

## Cholesky factorisation

`chol(A)` returns the lower-triangular `L` with a positive diagonal and `A = L·Lᵀ` — the
[Cholesky factorisation](https://en.wikipedia.org/wiki/Cholesky_decomposition) of a symmetric
positive-definite matrix. Like `expm` it is a single matrix, not a row, so `chol(A) * x` is an
ordinary matrix product.

```scala mdoc
Parser.parse("chol([[4, 2], [2, 3]])").get.eval(env).toExpression.toString
```

Its **refusal is as useful as its answer**: a matrix that is not symmetric, or not positive
definite (indefinite, negative definite, or singular), stays unevaluated — the cheapest test
there is that a quadratic form is strictly convex. An asymmetric matrix is never factored
from one triangle, which would answer for a different matrix. Both tests are **relative** to
the matrix's largest entry, so rescaling `A` never changes the verdict:

```scala mdoc
// indefinite: the eigenvalues are 3 and -1
Parser.parse("chol([[1, 2], [2, 1]])").get.eval(env).toExpression.toString
```

In exact mode the factor is exact even when a pivot's square root is irrational: each column of
`L` carries a single root, so the factor is made of surds — `chol([[4, 2], [2, 3]])` is
`[[2, 0], [1, √2]]` — and `L·Lᵀ` gives back `A` exactly.  `det`, `inv`, `lu` and `A^n` likewise
stay exact on a matrix whose surds share one root, and read two different roots at the working
precision.

## Singular values: `svd`, `pinv`, `rank`, `cond`, `nullspace`, `orth`

`svd(A)` returns the row `[[U, S, V]]` with `A = U·S·Vᵀ` — the
[singular value decomposition](https://en.wikipedia.org/wiki/Singular_value_decomposition), the
singular values on `S`'s diagonal, largest first. By default it is the **economy** form: with
`k = min(m, n)`, `U` is `m×k`, `S` `k×k` and `V` `n×k`, everything a computation multiplies with.
`svd(A, full)` completes `U` and `V` to square orthogonal matrices (and `S` to `m×n`), so the
extra columns span the left null space and the null space; `full` is a word of that slot only,
not a reserved name. Like `lu`, the factors bind by tuple assignment: `U, S, V := svd(A)`.

```scala mdoc
Parser.parse("svd([[3, 0], [4, 5]])").get.eval(env).toExpression.toString
```

The kernel is one-sided Jacobi, which works on `A`'s columns and never forms `AᵀA` (that would
square the condition number), and its answer is **certified**: a decomposition that does not
reconstruct `A` with orthonormal `U` and `V` stays unevaluated. An exact matrix is decomposed in
`Double`, as `lu` and `qr` are; a symbolic one stays symbolic.

Three answers are read off the singular values. `rank(A)` counts those above
`0.5·10⁻ᵖ·σ_max` at session precision `p` — the tolerance of `=` applied to the spectrum, and
**relative to the largest singular value**, so rescaling `A` never changes its rank;
`rank(A, tol)` takes a tolerance in `[0, 1)`, relative in the same way. `pinv(A)` is the
[Moore–Penrose pseudo-inverse](https://en.wikipedia.org/wiki/Moore%E2%80%93Penrose_inverse),
inverting exactly the singular values `rank` counts, so a rank-deficient matrix is not amplified
by rounding:

```scala mdoc
Parser.parse("rank([[1, 2], [2, 4]])").get.eval(env).toExpression.toString
```

```scala mdoc
Parser.parse("pinv([[1, 2], [2, 4]])").get.eval(env).toExpression.toString
```

`cond(A)` is the 2-norm condition number `σ_max/σ_min`, and `inf` for a singular matrix. It
takes a different cut-off from `rank`, on purpose: it reports how close to singular `A` is, so it
stays finite until the smallest singular value is lost in rounding (`max(m, n)·ε·σ_max`).

```scala mdoc
Parser.parse("cond([[1, 0], [0, 0.001]])").get.eval(env).toExpression.toString
```

Two bases are read off the same decomposition, at the same rank. `nullspace(A)` returns an
orthonormal basis of the null space `{x : A·x = 0}` as the columns of an `n×(n − rank)` matrix,
and `orth(A)` an orthonormal basis of the range as an `m×rank` one; both take `rank`'s optional
tolerance, so `cols(nullspace(A)) + cols(orth(A)) = n` always holds. A subspace with no basis —
the null space of a full-column-rank matrix, the range of the zero matrix — answers `false`, since
a matrix cannot have zero columns. A basis is unique only up to rotation, so check one by what it
does, not by its entries:

```scala mdoc
Parser.parse("nullspace([[1, 2, 3], [2, 4, 6], [1, 0, 1]])").get.eval(env).toExpression.toString
```

```scala mdoc
Parser.parse("nullspace([[1, 0], [0, 1]])").get.eval(env).toExpression.toString
```

The decomposition is bounded by `max(m, n)·min(m, n)² ≤ 10⁷` — a 200×200 or a 1000×100 matrix —
checked before any work; past it the call stays unevaluated and the REPL says why.

## Block matrices

`hcat` and `vcat` join any number of blocks side by side and stacked, `blkdiag` puts them on the
diagonal, `repmat(A, m, n)` tiles one, `kron(A, B)` is the
[Kronecker product](https://en.wikipedia.org/wiki/Kronecker_product), and `ones(m, n)` is the
all-ones counterpart of `zeros`:

```scala mdoc
Parser.parse("blkdiag([[1]], [[2, 3], [4, 5]])").get.eval(env).toExpression.toString
```

```scala mdoc
Parser.parse("kron(eye(2), [[1, 2]])").get.eval(env).toExpression.toString
```

`submatrix(A, r0, r1, c0, c1)` slices rows `r0..r1` and columns `c0..c1`, **1-based and
inclusive** — the convention `at(A, i, j)` uses, so the `1×1` slice `submatrix(A, i, i, j, j)`
holds exactly `at(A, i, j)`:

```scala mdoc
Parser.parse("submatrix([[1, 2, 3], [4, 5, 6], [7, 8, 9]], 2, 3, 1, 2)").get.eval(env).toExpression.toString
```

All of them rearrange cells through one shared assembly, so they work unchanged on numeric,
exact and symbolic matrices. In `hcat`, `vcat`, `blkdiag` and `kron` a **number counts as a
`1×1` block**; a name with no value yet is **never guessed** to be one, since it might later be
bound to a matrix of any shape, so the call waits:

```scala mdoc
Parser.parse("hcat(1, [[2]])").get.eval(env).toExpression.toString
Parser.parse("hcat(x, [[2]])").get.eval(env).toExpression.toString
```

Blocks that do not conform, and indices out of range or not whole, leave the call unevaluated;
a result above a million cells is refused before anything is built.
