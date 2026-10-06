---
title: Calculus
nav_order: 5
---

<img src="logo_bw.svg" alt="" style="height:80px;width:auto;float:right;margin:0 0 8px 16px"/>

# Calculus
<div style="clear:both"></div>

```scala mdoc:silent
import it.grypho.scala.leonardo.core.*
import it.grypho.scala.leonardo.scalar.*

val x = _Variable("x")
val env = new Environment()
```

## Symbolic differentiation

`derive(e, v)` applies the standard differentiation rules and returns a symbolic
expression. The
[chain rule](https://en.wikipedia.org/wiki/Chain_rule),
[product rule](https://en.wikipedia.org/wiki/Product_rule), and
[quotient rule](https://en.wikipedia.org/wiki/Quotient_rule) are all handled:

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

Beyond the linear chain rule, non-linear
**[u-substitution](https://en.wikipedia.org/wiki/Integration_by_substitution)** closes
`∫ f(g(x))·g'(x) dx`: the engine tries candidate inner functions `g`, divides the integrand
by `g'`, and integrates in `g` only when the quotient is free of `x`.

```scala mdoc
// ∫ x·e^(x²) dx = e^(x²)/2   (u = x²)
integrate(Product(x, Exp(Power(x, _Number(2.0)))), x).toString
```

Radical integrands close by
**[trigonometric](https://en.wikipedia.org/wiki/Trigonometric_substitution) /
[hyperbolic](https://en.wikipedia.org/wiki/Hyperbolic_substitution) substitution**;
`√(a²+x²)` and `√(x²−a²)` use the hyperbolic substitution, so the result is written with the
`asinh`/`acosh` functions:

```scala mdoc
// ∫ dx/√(x²+1) = asinh(x)
integrate(Ratio(_Number(1.0), Power(Sum(Power(x, _Number(2.0)), _Number(1.0)), _Number(0.5))), x).toString
```

Classic [non-elementary integrals](https://en.wikipedia.org/wiki/Nonelementary_integral)
are answered with their **named special functions** — the
[trigonometric integrals](https://en.wikipedia.org/wiki/Trigonometric_integral) `Si`/`Ci`,
the [exponential integral](https://en.wikipedia.org/wiki/Exponential_integral) `Ei`,
the [logarithmic integral](https://en.wikipedia.org/wiki/Logarithmic_integral_function) `li`,
the [Fresnel integrals](https://en.wikipedia.org/wiki/Fresnel_integral)
`fresnelS`/`fresnelC`, and the
[error function](https://en.wikipedia.org/wiki/Error_function) `erf` — symbolic nodes with
numeric kernels, so the antiderivative still evaluates:

```scala mdoc
// ∫ sin(x)/x dx = Si(x)
integrate(Ratio(Sin(x), x), x).toString
```

Unsupported forms are left as `_Integral` nodes (symbolic, not an error):

```scala mdoc
integrate(Sin(Product(x, x)), x).toString    // sin(x²) has no closed form
```

## Vector calculus

The [gradient](https://en.wikipedia.org/wiki/Gradient) `grad`,
[divergence](https://en.wikipedia.org/wiki/Divergence) `div`,
[curl](https://en.wikipedia.org/wiki/Curl_(mathematics)) `curl`,
[Laplacian](https://en.wikipedia.org/wiki/Laplace_operator) `laplacian`,
[Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant) `jacobian` and
[Hessian](https://en.wikipedia.org/wiki/Hessian_matrix) `hessian` take a scalar or vector
field followed by the **ordered coordinate tuple** — the order is never inferred, because it
fixes the order of the result's components.  A vector field is an n×1 matrix.

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

**Cartesian, [cylindrical](https://en.wikipedia.org/wiki/Cylindrical_coordinate_system) and
[spherical](https://en.wikipedia.org/wiki/Spherical_coordinate_system)** coordinates are all
supported, through *one* set of
[orthogonal-curvilinear](https://en.wikipedia.org/wiki/Orthogonal_coordinates) formulas
parameterised by the system's scale factors — Cartesian is simply the case where they are
all `1`:

```scala mdoc:silent
val r  = _Variable("r")
val th = _Variable("t")
val ph = _Variable("p")
val atPoint = new Environment(variables =
  Map("r" -> _Number(2.0), "t" -> _Number(0.7), "p" -> _Number(0.4)))
```

The [Newtonian potential](https://en.wikipedia.org/wiki/Newtonian_potential) is
[harmonic](https://en.wikipedia.org/wiki/Harmonic_function) away from the origin —
`∇²(1/r) = 0` — and that is a sharp check on the scale factors, since a single wrong one
breaks it.  The symbolic form does not visibly collapse (`simplify` does no common-factor
cancellation), so evaluate it:

```scala mdoc
_Laplacian(Ratio(_Number(1.0), r), Vector(r, th, ph), CoordinateSystem.Spherical).eval(atPoint)
```

`cylindrical` is `(r, θ, z)`; `spherical` is `(r, θ, φ)` with **θ the polar angle** (physics)
and `sphericalmaths` has **θ azimuthal and φ polar** (mathematics).  All are
three-dimensional, and the coordinates are identified by **position, not by name** — so the
two spherical keywords differ in argument *order*, not in naming.  Exchanging the last two
coordinates flips the handedness of the basis, so `curl`, a pseudo-vector, carries the
matching sign; the two conventions agree on the curl of the same physical field.

## Definite integration (Simpson's rule)

`_DefIntegral(e, v, lo, hi)` computes the definite integral numerically using composite
[Simpson's rule](https://en.wikipedia.org/wiki/Simpson%27s_rule).

The limits are **children**, not binders: only `v` is bound, so a limit may be any expression
and may name other variables.  That is what gives iterated integration with variable limits
(`integral(integral(x*y, y, 0, x), x, 0, 1)`) for free — the inner limit is an ordinary free
occurrence of the outer variable, bound per sample by the outer pass.  In the grammar the
limits are full expressions too, so `integral(sin(x), x, 0, 2*pi)` is written as it reads.

It uses a compiled `Double ⇒ Double` closure when the integrand is free of unresolvable
nodes — no per-step allocation:

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

## Systems of differential equations

`ode(rhs, y, t, t0, y0, target)` solves the initial-value problem `y' = rhs(t, y)`,
`y(t0) = y0`, at `target`.  When `y0` is a column the problem is a **system**: a
constant-coefficient linear one closes through the matrix exponential, and a **time-varying or
nonlinear** one is integrated numerically.  The state may be one name bound to the whole column
(`A(t)*y`, or components read with `at(y, i, 1)`) or a **column of names**, each bound to its
component, so a system reads as written:

```scala mdoc:silent
import it.grypho.scala.leonardo.parser.Parser
import it.grypho.scala.leonardo.ode.*

def run(s: String) = Parser.parse(s) match
  case Parser.Success(e, _) => e.eval(env)
  case other                => sys.error(other.toString)
```

```scala mdoc
// x'' = -x as x' = v, v' = -x, from (1, 0): the answer is (cos 1, -sin 1)
run("ode([[v], [-x]], [[x], [v]], t, 0, [[1], [0]], 1)")
```

```scala mdoc
// Lotka-Volterra predator-prey, nonlinear
run("ode([[x - x*y], [x*y - y]], [[x], [y]], t, 0, [[2], [1]], 5)")
```

The answer is **certified by step doubling**: fourth-order
[Runge–Kutta](https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods) runs with `n` and `2n`
steps, and the state is returned only when the two agree within the tolerance `=` uses
(`0.5·10^-precision`).  An explicit method on a
[stiff](https://en.wikipedia.org/wiki/Stiff_equation) system produces a finite, confident and
wrong number; here it produces a refusal instead, and the node's `detailed` says why — the reason
the REPL appends as a note:

```scala mdoc
Parser.parse("ode([[-1000000 * (y - cos(t))]], [[y]], t, 0, [[0]], 1)") match
  case Parser.Success(s: _ODESystem, _) => s.detailed(env)
  case other                            => sys.error(other.toString)
```

The integrator is also public as a **step** and a **trajectory**, the method always named —
`euler`, `rk4`, or the adaptive
[Dormand–Prince](https://en.wikipedia.org/wiki/Dormand%E2%80%93Prince_method) `rk45`:
`odeStep(F, y, t, t0, y0, h, method)` is the state at `t0 + h`, and
`odeSolve(F, y, t, t0, y0, t1, h, method)` the trajectory, one row `[t, state…]` per output time,
`t1` always the last.  These are **not** certified: the step is the caller's choice, which is
what a controller simulation wants.  For `rk45`, `h` is the spacing of the output rows and the
steps between them are chosen to hold the tolerance.

```scala mdoc
run("odeSolve([[v], [-x]], [[x], [v]], t, 0, [[1], [0]], 0.3, 0.1, rk4)")
```
