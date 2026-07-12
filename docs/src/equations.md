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
// log(-1) = iπ
Log(_Number(-1.0)).eval(env)
```

### Parsing complex literals

`3i` and `2 + 3i` parse directly:

```scala mdoc
Parser.parse("2 + 3i").get.eval(env)
```

```scala mdoc
Parser.parse("(1 + i)^4").get.eval(env)
```
