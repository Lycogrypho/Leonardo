---
title: Equations & Complex Numbers
---

<img src="logo_bw.svg" alt="" height="80" style="float:right;margin:0 0 8px 16px"/>

# Equations & Complex Numbers
<div style="clear:both"></div>

```scala mdoc:silent
import it.grypho.scala.leonardo.core.*
import it.grypho.scala.leonardo.scalar.*
import it.grypho.scala.leonardo.equation.*
import it.grypho.scala.leonardo.parser.Parser

val x = _Variable("x")
val env = new Environment()
```

## Equations

An `_Equation(lhs, rhs)` is a relation node. When both sides reduce to concrete
values, `eval` returns a `_Bool` using tolerance-based equality
(`|a − b| ≤ 0.5 × 10⁻ᵖ` at precision `p`), so floating-point noise does not
produce false negatives:

```scala mdoc
// 2x = 6 at x = 3 → true
val eq1 = _Equation(Product(_Number(2.0), x), _Number(6.0))
val envX3 = new Environment(5, Map("x" -> _Number(3.0)))
eq1.eval(envX3)
```

```scala mdoc
// sin(π) = 0 — true despite floating-point noise
_Equation(Sin(_Number(math.Pi)), _Number(0.0)).eval(env)
```

Equations with free variables stay symbolic:

```scala mdoc
eq1.eval(env)
```

### Equality check (`==`)

`_EqualityCheck` has the same semantics as `_Equation` but is not accepted by
`solve`. Use it when you want a boolean result without the intent to solve:

```scala mdoc
Parser.parse("x^2 == x * x").get.eval(envX3)
```

## Solving equations

All solve examples use the `solve(…, v)` grammar, which builds a `_Solve` node
evaluated by calling the underlying `solve` algorithm:

### Linear equations

```scala mdoc
// 2x + 3 = 7  →  x = 2
Parser.parse("solve(2*x + 3 = 7, x)").get.eval(env)
```

```scala mdoc
// Inline: a*x + b = 0 with concrete a, b
Parser.parse("solve(3*x - 6 = 0, x)").get.eval(env)
```

### Quadratic equations

The discriminant is computed; 0, 1, or 2 real roots are returned:

```scala mdoc
// x² - 5x + 6 = 0  →  x = 2, x = 3
Parser.parse("solve(x^2 - 5*x + 6 = 0, x)").get.eval(env)
```

```scala mdoc
// x² + 1 = 0  →  no real roots
Parser.parse("solve(x^2 + 1 = 0, x)").get.eval(env)
```

### Numeric fallback

Transcendental and higher-degree equations fall back to a sign-change scan
over `[-100, 100]` followed by bisection (up to 8 roots):

```scala mdoc
// sin(x) = 0.5  — first root in [-100, 100]
Parser.parse("solve(sin(x) = 0.5, x)").get.eval(env)
```

### Matrix equations (scalar unknown)

When both sides are matrices, `solve` finds a scalar unknown by decomposing the
equation into its per-cell scalar equations and keeping the value that satisfies
**every** cell (the intersection of the per-cell solution sets):

```scala mdoc
// each cell agrees on x = 3
Parser.parse("solve([[x, 2*x]] = [[3, 6]], x)").get.eval(env)
```

An inconsistent system — where different cells demand different values — has no
solution, so the node stays symbolic:

```scala mdoc
// cell 1 wants x = 1, cell 2 wants x = 2 → no common root
Parser.parse("solve([[x, x]] = [[1, 2]], x)").get.eval(env)
```

### Matrix equations (matrix unknown)

When the unknown is itself a **matrix** in a linear equation `A·X = B` (or `X·A = B`),
`solve` returns the unique solution `X = A⁻¹·B` via the inverse kernel. The known
matrices must be bound so the parser sees a plain product (a bare `X` is a scalar
variable syntactically):

```scala mdoc
val A = _MatrixValue(2, 2, Array(2.0, 0.0, 0.0, 2.0))
val B = _MatrixValue(2, 2, Array(4.0, 6.0, 8.0, 10.0))
val mEnv = new Environment(5, Map("A" -> A, "B" -> B))
// A·X = B  →  X = A⁻¹·B = [[2, 3], [4, 5]]
Parser.parse("solve(A * X = B, X)").get.eval(mEnv)
```

A singular (non-invertible) `A` has no unique solution and stays symbolic.

Beyond the one-sided shapes, `solve` also recognises an **affine term** and a
**two-sided product**. A constant matrix added on the unknown's side is peeled to
the right (`A·X + C = B` → `A·X = B − C`), and a matrix on each flank is handled by
inverting both (`A·X·D = B` → `X = A⁻¹·B·D⁻¹`):

```scala mdoc
val D = _MatrixValue(2, 2, Array(5.0, 0.0, 0.0, 5.0))
val B2 = _MatrixValue(2, 2, Array(10.0, 20.0, 30.0, 40.0))
val twoSidedEnv = new Environment(5, Map("A" -> A, "D" -> D, "B" -> B2))
// A·X·D = B  →  X = A⁻¹·B·D⁻¹ = [[1, 2], [3, 4]]
Parser.parse("solve(A * X * D = B, X)").get.eval(twoSidedEnv)
```

The coefficients may be symbolic `_Matrix` literals with free variables, in which
case the solution is a symbolic matrix expression (via cofactor expansion, capped at
6×6).

**General linear matrix equations** — the unknown appearing in several terms with
coefficients on different sides — are solved by Kronecker vectorization: each term
`s·L·X·R` contributes `s·(Rᵀ ⊗ L)` to a dense system over `vec(X)`. This covers the
Sylvester equation `A·X + X·B = C`, the Lyapunov equation `A·X + X·Aᵀ = C`, and
scalar coefficients like `2·X = B`:

```scala mdoc
// Sylvester: A·X + X·B = C with X = [[1, 0], [2, 1]] → C = [[9, 3], [14, 10]]
val sA = _MatrixValue(2, 2, Array(1.0, 2.0, 0.0, 3.0))
val sB = _MatrixValue(2, 2, Array(4.0, 1.0, 0.0, 5.0))
val sC = _MatrixValue(2, 2, Array(9.0, 3.0, 14.0, 10.0))
val sylvesterEnv = new Environment(5, Map("A" -> sA, "B" -> sB, "C" -> sC))
Parser.parse("solve(A * X + X * B = C, X)").get.eval(sylvesterEnv)
```

The system is solvable only when it is non-singular — for Sylvester, when `A` and
`−B` share no eigenvalue; otherwise the equation has no (unique) solution and the
solve node stays symbolic. Coefficients must reduce to dense matrices for this tier.

### Linear systems

`solveSystem([[eq₁, eq₂, …]], v₁, v₂, …)` solves a square system via
Gaussian elimination (partial pivoting for concrete coefficients, symbolic
row reduction otherwise):

```scala mdoc
// x + y = 3, x - y = 1  →  x = 2, y = 1
Parser.parse("solveSystem([[x + y = 3, x - y = 1]], x, y)").get.eval(env)
```

## Complex numbers

The imaginary unit `i` is a built-in constant (`_Complex(0, 1)`). Arithmetic
closes over the complex field:

```scala mdoc:silent
val i = _Complex.of(0, 1)
```

```scala mdoc
// i² = -1
Product(i, i).eval(env)
```

```scala mdoc
// (2 + 3i)(1 - i) = 5 + i
Product(_Complex.of(2, 3), _Complex.of(1, -1)).eval(env)
```

### Euler's identity

```scala mdoc
// e^(iπ) = -1
Exp(Product(i, _Number(math.Pi))).eval(env)
```

### Principal complex roots and logarithms

Negative bases with fractional exponents and logarithms of negative reals
return their principal complex values rather than staying symbolic:

```scala mdoc
// √(-2) = i√2
Power(_Number(-2.0), _Number(0.5)).eval(env)
```

```scala mdoc
// ln(-1) = iπ
Ln(_Number(-1.0)).eval(env)
```

### Parsing complex literals

`3i` and `2 + 3i` parse directly:

```scala mdoc
Parser.parse("2 + 3i").get.eval(env)
```

```scala mdoc
Parser.parse("(1 + i)^4").get.eval(env)
```
