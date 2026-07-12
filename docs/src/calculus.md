---
title: Calculus
---

<img src="logo_bw.svg" alt="" height="80" style="float:right;margin:0 0 8px 16px"/>

# Calculus
<div style="clear:both"></div>

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

Unsupported forms are left as `_Integral` nodes (symbolic, not an error):

```scala mdoc
integrate(Sin(Product(x, x)), x).toString    // sin(x²) has no closed form
```

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
pts.map { case (xi, yi) => f"($xi%.4f, $yi%.4f)" }
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
