---
title: Calculus
nav_order: 4
---

# Calculus

```scala mdoc:silent
import it.grypho.scala.leonardo.core.*
import it.grypho.scala.leonardo.scalar.*

val x = _Variable("x")
val env = new Environment()
```

## Symbolic differentiation

`derive(e, v)` applies standard differentiation rules and returns a symbolic
expression. Chain rule, product rule, and quotient rule are all handled:

```scala mdoc
derive(Power(x, _Number(3.0)), x).toString
```

```scala mdoc
// d/dx sin(x^2) = cos(x^2) * 2x   (chain rule)
derive(Sin(Power(x, _Number(2.0))), x).toString
```

```scala mdoc
// d/dx (x * exp(x)) = exp(x) + x*exp(x)   (product rule)
derive(Product(x, Exp(x)), x).toString
```

Combine with `simplify` or `simplifyFully` to reduce the result:

```scala mdoc
simplifyFully(derive(Product(x, Exp(x)), x)).toString
```

The hyperbolic and reciprocal-trigonometric functions (`sinh`/`cosh`/`tanh`,
`asinh`/`acosh`/`atanh`, `sec`/`csc`/`cot`, `sech`/`csch`/`coth`) are first-class
nodes with the standard rules:

```scala mdoc
// d/dx tanh(x) = sech(x)^2
derive(Tanh(x), x).toString
```

### Higher-order derivatives

Wrap the result in a second `derive` call (or use `_Derivative` nodes):

```scala mdoc
// d²/dx² x^4 = 12x²
val d2 = derive(derive(Power(x, _Number(4.0)), x), x)
simplifyFully(d2).toString
```

### Evaluating a derivative numerically

```scala mdoc
val slope = derive(Sin(x), x)   // should be cos(x)
val envPi2 = new Environment(5, Map("x" -> _Number(math.Pi / 2)))
slope.eval(envPi2)               // cos(π/2) ≈ 0
```

## Indefinite integration

`integrate(e, v)` applies the symbolic rule table (linearity, power rule,
`exp`/`sin`/`cos`, `1/x → log`, linear-argument chain rule):

```scala mdoc
integrate(Power(x, _Number(2.0)), x).toString
```

```scala mdoc
integrate(Sin(x), x).toString
```

```scala mdoc
// ∫ 1/x dx = log(x)
integrate(Ratio(_Number(1.0), x), x).toString
```

```scala mdoc
// Chain rule: ∫ sin(3x) dx = -cos(3x)/3
integrate(Sin(Product(_Number(3.0), x)), x).toString
```

Beyond the linear chain rule, non-linear **u-substitution** closes `∫ f(g(x))·g'(x) dx`:
the engine tries candidate inner functions `g`, divides the integrand by `g'`, and
integrates in `g` only when the quotient is free of `x`.

```scala mdoc
// ∫ x·e^(x²) dx = e^(x²)/2   (u = x²)
integrate(Product(x, Exp(Power(x, _Number(2.0)))), x).toString
```

Radical integrands close by **trigonometric / hyperbolic substitution**; `√(a²+x²)` and
`√(x²−a²)` use the hyperbolic substitution, so the result is written with the `asinh`/`acosh`
functions:

```scala mdoc
// ∫ dx/√(x²+1) = asinh(x)
integrate(Ratio(_Number(1.0), Power(Sum(Power(x, _Number(2.0)), _Number(1.0)), _Number(0.5))), x).toString
```

Classic non-elementary integrals are answered with their **named special functions**
(`Si`, `Ci`, `Ei`, `li`, `fresnelS`, `fresnelC`, `erf`) — symbolic nodes with numeric
kernels, so the antiderivative still evaluates:

```scala mdoc
// ∫ sin(x)/x dx = Si(x)
integrate(Ratio(Sin(x), x), x).toString
```

Unsupported forms are left as `_Integral` nodes (symbolic, not an error):

```scala mdoc
integrate(Sin(Product(x, x)), x).toString    // sin(x²) has no closed form
```

## Vector calculus

`grad`, `div`, `curl`, `laplacian`, `jacobian` and `hessian` take a scalar or vector field
followed by the **ordered coordinate tuple** — the order is never inferred, because it fixes
the order of the result's components.  A vector field is an n×1 matrix.

```scala mdoc
import it.grypho.scala.leonardo.vector.*
import it.grypho.scala.leonardo.matrix._Matrix

val y = _Variable("y")
// grad(x^2 * y) = [2xy, x^2]^T
_Grad(Product(Power(x, _Number(2.0)), y), Vector(x, y)).eval(env).toExpression.toString
```

The coordinate-free identities hold, which makes them the natural property tests:
`curl(grad f) = 0`, `div(curl F) = 0`, and `laplacian` is *defined* as `div ∘ grad` so the two
cannot disagree.  Shapes that have no meaning — a `curl` outside three dimensions, a
component/coordinate count mismatch, a repeated coordinate — stay symbolic rather than being
guessed.

**Cartesian, cylindrical and spherical** coordinates are all supported, through *one* set of
orthogonal-curvilinear formulas parameterised by the system's scale factors — Cartesian is
simply the case where they are all `1`:

```scala mdoc:silent
val r  = _Variable("r")
val th = _Variable("t")
val ph = _Variable("p")
val atPoint = new Environment(variables =
  Map("r" -> _Number(2.0), "t" -> _Number(0.7), "p" -> _Number(0.4)))
```

The Newtonian potential is harmonic away from the origin — `∇²(1/r) = 0` — and that is a
sharp check on the scale factors, since a single wrong one breaks it.  The symbolic form does
not visibly collapse (`simplify` does no common-factor cancellation), so evaluate it:

```scala mdoc
_Laplacian(Ratio(_Number(1.0), r), Vector(r, th, ph), CoordinateSystem.Spherical).eval(atPoint)
```

`cylindrical` is `(r, θ, z)` and `spherical` is `(r, θ, φ)` with **θ the polar angle** (the
physics convention).  Both are three-dimensional, and the coordinates are identified by
**position, not by name**.

## Definite integration (Simpson's rule)

`_DefIntegral(e, v, lo, hi)` computes the definite integral numerically using
adaptive Simpson's rule. It uses a compiled `Double ⇒ Double` closure when
the integrand is free of unresolvable nodes — no per-step allocation:

```scala mdoc
// ∫₀¹ x² dx = 1/3
_DefIntegral(Power(x, _Number(2.0)), x, _Number(0.0), _Number(1.0)).eval(env)
```

```scala mdoc
// ∫₀π sin(x) dx = 2
_DefIntegral(Sin(x), x, _Number(0.0), _Number(math.Pi)).eval(env)
```

```scala mdoc
// ∫₁ᵉ 1/x dx = 1  (ln e − ln 1)
_DefIntegral(Ratio(_Number(1.0), x), x, _Number(1.0), _Number(math.E)).eval(env)
```

## Function sampling

`sample(e, v, lo, hi, n, env)` evaluates an expression over a uniform grid of
`n` points in `[lo, hi]`, returning `Vector[(Double, Double)]` with non-finite
results silently dropped:

```scala mdoc:silent
val pts = sample(Sin(x), x, 0.0, math.Pi, 5, env)
```

```scala mdoc
pts.length
```

```scala mdoc
pts.map { (xi: Double, yi: Double) => f"($xi%.4f, $yi%.4f)" }
```

The fast path compiles the expression to a `Double ⇒ Double` closure (no
per-step allocation). A fallback per-step `eval` handles non-compilable nodes
like `_Derivative`.

The Syntax extension gives method-call form:

```scala mdoc:silent
import it.grypho.scala.leonardo.scalar.Syntax.*
val pts2 = Sin(x).sample(x, 0.0, math.Pi, 5, env)
```

```scala mdoc
pts2 == pts
```
