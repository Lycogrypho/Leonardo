---
title: Optimization
nav_order: 11
---

<img src="logo_bw.svg" alt="" style="height:80px;width:auto;float:right;margin:0 0 8px 16px"/>

# Optimization
<div style="clear:both"></div>

Most optimization software is numeric: give it a starting point, get back a number. Leonardo
does that too, but its first job is the part a numeric library cannot do — **derive the
conditions an optimum must satisfy and solve them exactly**: every stationary point, its
classification, whether the function is convex at all, the Lagrange and
[Karush–Kuhn–Tucker](https://en.wikipedia.org/wiki/Karush%E2%80%93Kuhn%E2%80%93Tucker_conditions)
conditions.

One rule runs through the whole page: **an answer Leonardo cannot prove complete is not
given.** A list of stationary points is read as *all* of them, so a list missing one is a
confident wrong answer. Where the solver cannot be sure, the expression stays symbolic.

```scala mdoc:silent
import it.grypho.scala.leonardo.core.*
import it.grypho.scala.leonardo.optimize.*
import it.grypho.scala.leonardo.parser.Parser

val env = new Environment()
val x   = _Variable("x")
val y   = _Variable("y")

def e(src: String): _Expression = Parser.parse(src).get

// What the language prints for a call: the answer, or the call itself when it declines.
def run(src: String): String = e(src).eval(env).toExpression.toString
```

```scala mdoc:invisible
// BUILD-TIME SCAFFOLDING: executes the REPL transcripts below, so they are the REPL's own output.
import it.grypho.scala.leonardo.docs.Lesson
val lesson = Lesson()
```

## Stationary points

`stationary(f, x, y, …)` solves `∇f = 0`. The answer has one row per point:

```scala mdoc
run("stationary(x^3 - 3*x + y^2, x, y)")
```

The gradient here is nonlinear, which is why this needs more than a linear-system solver.
Leonardo **eliminates**: it solves an equation that is linear in one unknown, or that has only
one unknown left, substitutes, and repeats. The subtle step is dividing by a coefficient that
depends on the other unknowns — the points where that coefficient is zero would silently
disappear, so the solver also follows that branch. Here, solving the first gradient component
for `x` divides by `2y`, and the branch `y = 0` is the one that finds `(0, 0)` and `(3, 0)`:

```scala mdoc
run("stationary(x*y*(x + y - 3), x, y)")
```

When the set of points cannot be listed — a whole line of them, or infinitely many — the call
stays as it was written:

```scala mdoc
run("stationary((x - y)^2, x, y)")
run("stationary(sin(x), x)")
```

and `false` means there are provably none:

```scala mdoc
run("stationary(x + y, x, y)")
```

### Classifying a point

The [second-order test](https://en.wikipedia.org/wiki/Second_partial_derivative_test) reads the
signs of the Hessian's eigenvalues at the point:

```scala mdoc:silent
val f = e("x^3 - 3*x + y^2")
```

```scala mdoc
classifyStationary(f, Vector(x, y), Map("x" -> 1.0, "y" -> 0.0), env)
classifyStationary(f, Vector(x, y), Map("x" -> -1.0, "y" -> 0.0), env)
classifyStationary(e("x^4"), Vector(x), Map("x" -> 0.0), env)
```

`Degenerate` is not a failure: at a singular Hessian the test genuinely cannot decide — `x⁴`
has a minimum at the origin and `x³` an inflection, with the same zero Hessian. At the REPL the
classification is appended to `stationary`'s answer as a note:

```scala mdoc:passthrough
lesson.show("stationary(x^3 - 3*x + y^2, x, y)")
```

Unlike `solve`, `stationary` does not bind its answer: several points cannot all be the value
of `x`.

## Convexity

`convex(f, x, y, …)` answers `true` only with a proof and `false` only with a witness:

```scala mdoc
run("convex(x^2 + y^2, x, y)")
run("convex(x^4 + y^2, x, y)")
run("convex(x*y, x, y)")
```

The proof is that **every principal minor** of the symbolic Hessian is non-negative. Not just
the leading ones: those prove a matrix *definite*, and convexity needs only *semidefinite* —
`diag(0, −2)` has leading minors `0` and `0` and is not convex.

Two refusals are worth knowing. `1/x²` has a positive second derivative everywhere it exists,
but it does not exist at `0`, and a function on a domain with a hole is not convex; and `x·ln x`
is negatively curved at `x = −1`, where it is not a real number — so neither gets an answer:

```scala mdoc
run("convex(1/x^2, x)")
run("convex(x*ln(x), x)")
```

## Equality constraints

`lagrange(f, [[g1], [g2]], x, y, …)` finds the stationary points of `f` on the surface
`g₁ = 0, g₂ = 0, …` by
[Lagrange multipliers](https://en.wikipedia.org/wiki/Lagrange_multiplier). A constraint may be
written as an expression meaning `= 0`, or as an equation; a single one needs no matrix:

```scala mdoc
run("lagrange(x + y, [[x^2 + y^2 - 2]], x, y)")
run("lagrange(x + y, x^2 + y^2 = 2, x, y)")
```

The multipliers are solved for and then dropped from the answer.

## Inequality constraints

`kkt(f, g, h, x, y, …)` **states** the Karush–Kuhn–Tucker conditions of

> minimise `f` subject to `g ≤ 0` and `h = 0`

as one conjunction you can read, check, or hand to `solve`. The convention — minimisation,
`g ≤ 0` — matters, because the sign of every multiplier depends on it. Pass `0` for a group
you do not have:

```scala mdoc:silent
val conditions = e("kkt(x^2 + y^2, 1 - x - y, 0, x, y)").eval(env).toExpression
```

```scala mdoc
conditions.toString
```

The multiplier is named `mu0` here, avoiding any name the problem already uses. The conditions
hold exactly at the optimum `(1/2, 1/2)` with `mu0 = 1`:

```scala mdoc
conditions.eval(env.withBinding("x", _Number(0.5)).withBinding("y", _Number(0.5))
                   .withBinding("mu0", _Number(1)))
```

Solving them is library API, by trying every set of active constraints:

```scala mdoc
kktPoints(e("x^2 + y^2"), e("1 - x - y"), e("0"), Vector(x, y), env)
```

A KKT point is a *candidate*: necessary for a minimum, not sufficient on a non-convex problem.

## Numerical minimisation

When there is no closed form, `minimize` iterates from a starting point. The variables are
written as a column — the shape of the starting point and of the answer — and the method is
**always named**, because two methods can stop at different points of the same function:

```scala mdoc
run("minimize((1 - x)^2 + 100*(y - x^2)^2, [[x], [y]], [[-1.2], [1]], bfgs)")
```

| method | what it is |
|---|---|
| `gd` | steepest descent with a backtracking line search |
| `newton` | Newton's method on the symbolic Hessian; declines where the Hessian is not positive definite |
| `bfgs` | BFGS with a strong-Wolfe line search |
| `pbfgs` | **projected** BFGS, for box constraints |

Bounds are two more columns before the method, with `inf` where there is none:

```scala mdoc
run("minimize((1 - x)^2 + 100*(y - x^2)^2, [[x], [y]], [[-1.2], [1]], [[-inf], [-inf]], [[0.5], [inf]], pbfgs)")
```

**An answer is certified.** The iteration runs until the gradient is negligible, and the point
is then checked against the tolerance `=` uses at the current precision — under bounds, the
*projected* gradient, which vanishes at a constrained minimum where the gradient does not. A
point that fails is not returned. Instead the call stays symbolic, and the REPL says why:

```scala mdoc:passthrough
lesson.show("minimize(x + y, [[x], [y]], [[0], [0]], bfgs)")
```

The same reason is available to a program as a `MinimizeFailure`:

```scala mdoc
minimize(e("x^2 - y^2"), Vector(x, y), Vector(0.5, 0.5), None, MinimizeMethod.Newton, env)
```

## Where to go next

- The full grammar of every call is on the [cheat sheet](cheatsheet.md#optimization).
- The design decisions — why elimination rather than a linear solver, why projected BFGS rather
  than L-BFGS-B — are summarised in the [architecture](architecture.md) page.
