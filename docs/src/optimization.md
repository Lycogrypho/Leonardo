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

When every component of the gradient couples the unknowns non-linearly, no such step applies.
The solver then computes a lexicographic
[Gröbner basis](https://en.wikipedia.org/wiki/Gr%C3%B6bner_basis) of the same equations, in
exact rational arithmetic. A lexicographic basis is triangular: one equation in the last unknown
alone, the others bringing in one unknown at a time, which the steps above then finish. Here
both components are cubic in `x` and `y`, and all nine points come back:

```scala mdoc
run("stationary(x^4 + y^4 + x^2*y^2 - 2*x^2 - 2*y^2, x, y)")
```

The basis is tried only where the steps give up, so it never changes an answer they give. It is
capped in size, degree, coefficient length and work, because exact Buchberger's algorithm can
grow without bound; past a cap the call declines.

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

**An answer is certified.** The iteration runs until the point is stationary to a tight target,
and it is then checked against the tolerance `=` uses at the current precision. Stationarity is
measured as a **pure number** — the change the gradient still predicts over the distance
travelled, relative to the decrease achieved — so the verdict does not depend on the units of
`f` or of any variable: `1e-10*(x - 3)^2` and `1e10*(x - 3)^2` are answered alike, where a bound
on the gradient's size would certify the first at its starting point. Under bounds it reads the
*projected* gradient, which vanishes at a constrained minimum where the gradient does not. A
point that fails is not returned. Instead the call stays symbolic, and the REPL says why:

```scala mdoc:passthrough
lesson.show("minimize(x + y, [[x], [y]], [[0], [0]], bfgs)")
```

The same reason is available to a program as a `MinimizeFailure`:

```scala mdoc
minimize(e("x^2 - y^2"), Vector(x, y), Vector(0.5, 0.5), None, MinimizeMethod.Newton, env)
```

## Quadratic programming

`qp(H, f, A, b, lb, ub)` minimises **`½ zᵀHz + fᵀz` subject to `A z ≤ b` and `lb ≤ z ≤ ub`**
— the `½` and the `≤` stated first, because the literature writes both conventions and a
silent choice would make every answer wrong for half its readers. `qp(H, f, A, b, Aeq, beq, lb,
ub)` adds `Aeq z = beq`. A group that is absent is written `0` (the `kkt` convention), an absent
bound `-inf` / `inf`, and a number for `lb` or `ub` applies to every variable. It is the problem
a linear model-predictive controller solves every cycle.

```scala mdoc:passthrough
lesson.show("qp([[1, -1], [-1, 2]], [[-2], [-6]], [[1, 1], [-1, 2], [2, 1]], [[2], [2], [3]], 0, inf)")
```

The note names the constraints holding with equality at the answer — which input is saturated,
which limit binds — the information an experimenter wants from the active set.

For a **positive definite** `H` — every MPC Hessian `ΓᵀQΓ + R` with `R` definite — the solver
is the **Goldfarb–Idnani dual active-set method**: it starts from the unconstrained minimum,
needs no feasible starting point, and finds infeasibility by itself. A merely **semidefinite**
`H`, where some direction costs nothing, goes to the **primal active-set method** instead. It
starts from a feasible point found by the exact linear program below, and follows a direction
of zero curvature until a constraint stops it, or reports the problem `Unbounded` when none
does. `H = 0` is a linear program. Neither method regularises `H`, which would solve a
different problem. Because that start comes from the exact linear program, **a semidefinite `qp`
inherits `lp`'s caps** — 40 variables, 80 rows of `A` and `Aeq`, coefficients of 2048 bits —
where a definite one accepts 500 variables and 2000 constraints; past them it declines and says
that the linear program did. Like `minimize`, an answer is **certified**: the KKT conditions are checked,
relative to the problem's scale, against the tolerance `=` uses, and a point that fails is not
returned.

```scala mdoc:passthrough
lesson.show("qp(eye(2), [[0], [0]], [[1, 0], [-1, 0]], [[-1], [-1]], -inf, inf)",
            "qp([[1, 0], [0, 0]], [[-2], [-1]], [[1, 1]], [[3]], 0, inf)",
            "qp([[1, 0], [0, 0]], [[0], [-1]], 0, 0, -inf, inf)")
```

A program calls `qpSolve`, which returns the active set with the answer; passing it back as
`warm` for the next, nearly identical problem saves the iterations (never the answer, which is
unique). That is how a control loop uses it; the REPL has no loop, so it does not offer it.

```scala mdoc
val h = _MatrixValue(2, 2, Array(1.0, -1.0, -1.0, 2.0))
val a = Some((_MatrixValue(3, 2, Array(1.0, 1.0, -1.0, 2.0, 2.0, 1.0)), Vector(2.0, 2.0, 3.0)))
val inf = Double.PositiveInfinity
val first = qpSolve(h, Vector(-2.0, -6.0), a, None, Vector(0.0, 0.0), Vector(inf, inf), Set.empty, env)
val next  = qpSolve(h, Vector(-2.1, -5.9), a, None, Vector(0.0, 0.0), Vector(inf, inf),
                    first.toOption.fold(Set.empty[QpConstraint])(_.active.toSet), env)
```

## Nonlinear least squares

`lsqfit` finds the parameters that make a model fit data best — the ones minimising the sum of
squared residuals `‖r(p)‖²`. The **curve-fit form** takes the model, its independent variable,
the data and the parameters as a column of names with a starting point:

```scala mdoc:passthrough
lesson.show("T := transpose(tabulate(k, k, 0, 10))",
            "Y := transpose(tabulate(2*exp(-0.5*k), k, 0, 10))",
            "lsqfit(a*exp(b*t), t, T, Y, [[a], [b]], [[1], [0]])")
```

The **residual form** takes the residual column itself, for a fit that is not one curve through
one data set. Over data it is written cell by cell, because `Y - a*exp(b*T)` with `T` a column
stays an unevaluated product:

```scala mdoc:passthrough
lesson.show("lsqfit(transpose(tabulate(at(Y, k, 1) - a*exp(b*at(T, k, 1)), k, 1, 11)), [[a], [b]], [[1], [0]])")
```

Bounds are two more arguments (`-inf`/`inf` for none, a number applying to every parameter),
and the trailing word `cov` answers the row `[[p, C]]` with the covariance
`C = σ²·(RᵀR)⁻¹`, `σ² = ‖r‖²/(m − n)`, for tuple assignment. There is none — `false` in its
place — when there are as many residuals as parameters, or when a bound is active, since the
linearised approximation it rests on does not hold at a boundary.

**How it is solved, and when it answers.** Levenberg–Marquardt, each step solved by QR of the
augmented Jacobian — never `JᵀJ`, which squares the condition number — and scaled by the
Jacobian's column norms, so the iteration is the same whatever units a parameter is written in.
A point is answered only if every Jacobian column is orthogonal to the residual within the
precision's tolerance (a cosine, so the test has no units either), or the residual is at the
rounding level of an exact fit. A far start that stalls is declined, never answered; so is a
fit whose parameters are not **identifiable** — a residual that depends on `a + b` only has a
whole line of minimisers, and any one point would be arbitrary:

```scala mdoc:passthrough
lesson.show("lsqfit((a + b)*t, t, T, Y, [[a], [b]], [[1], [1]])")
```

A program calls `curveFit` or `lsqfit`, whose `LsqResult` carries the iterations, the residual
norm, the covariance and which parameters a bound holds.

## Linear programming

`lp(f, A, b, lb, ub)` minimises **`fᵀx` subject to `A x ≤ b` and `lb ≤ x ≤ ub`**, `qp`'s shape
without the Hessian: an absent group is `0`, an absent bound `-inf` / `inf`, and `lp(f, A, b, Aeq,
beq, lb, ub)` adds equalities. The note gives the optimal value and the binding constraints:

```scala mdoc:passthrough
lesson.show("lp([[-3], [-5]], [[1, 0], [0, 2], [3, 2]], [[4], [12], [18]], 0, inf)")
```

That is `max 3x + 5y`, written as a minimisation of its negative. **It is solved exactly**: the
simplex method runs over the rationals, so every decision a floating-point simplex makes with a
tolerance — is this reduced cost negative, is this ratio a tie — is made exactly. An infeasible
or an unbounded problem is therefore *proved* so, not suspected; Bland's pivoting rule rules
out cycling on a degenerate problem; and in exact mode the answer is exact:

```scala mdoc:passthrough
lesson.show("exact on", "lp([[-1], [-1]], [[3, 1], [1, 3]], [[1], [1]], 0, inf)", "exact off",
            "lp(1, [[1], [-1]], [[1], [-2]], -inf, inf)")
```

The cost of exactness is the size the rationals grow to, so the call is capped at 40 variables,
80 constraints and coefficients of 2048 bits; past those it declines and says so. When the
optimum is not unique, the vertex Bland's rule reaches is returned. A program calls `lpSolve`,
whose `LpResult` carries the value and the active set.

## Constrained nonlinear programming

`nlp(f, [[x], [y]], x0, g, h, method)` minimises `f` subject to **`g ≤ 0` and `h = 0`** — the
`kkt` convention, minimisation with `g ≤ 0` — from the starting point `x0`. Each group is a
column, one constraint, or `0` when there is none, and a cell may be written as a relation;
`lb` and `ub` before the method add bounds. The method is always named. `sqp` is sequential
quadratic programming: one `qp` per step on the constraints linearised at the current point. The
other, `auglag`, is an augmented Lagrangian, minimising a penalised objective with multiplier
estimates that improve each round. The acceptance problem of the MPC requirement, `min x + y` on
the disc `x² + y² ≤ 2`:

```scala mdoc:passthrough
lesson.show("nlp(x + y, [[x], [y]], [[1], [0]], x^2 + y^2 <= 2, 0, sqp)")
```

The note names the inequality that binds and its multiplier `μ`, the sensitivity of the
optimum to that constraint: `∇f + μ∇g = 0` there, with `μ ≥ 0`. Equalities and bounds combine:

```scala mdoc:passthrough
lesson.show("nlp(x^2 + 2*y^2, [[x], [y]], [[0], [0]], 0, x + y = 3, auglag)",
            "nlp((1 - x)^2 + 100*(y - x^2)^2, [[x], [y]], [[-1.2], [1]], 0, 0, -inf, [[0.5], [inf]], sqp)")
```

**What is answered is a *local* KKT point**, as with `minimize`: on a non-convex problem the
two methods, or two starting points, may stop at different ones. Either way the point is
**certified** before it is returned: stationarity of the Lagrangian, feasibility, `μ ≥ 0` and
complementarity, each measured against the terms it is made of, so the verdict does not change
when the objective or a variable is rescaled. SQP keeps its Hessian positive definite by a
damped BFGS update, so every subproblem is a `qp` the Goldfarb–Idnani solver accepts. A
problem whose constraints admit no point is declined, not answered:

```scala mdoc:passthrough
lesson.show("nlp(x^2, [[x]], 0, [[x + 1], [1 - x]], 0, sqp)")
```

A program calls `nlpSolve`, whose `NlpResult` carries the multipliers of both groups and the
active inequalities; `qpSolve`'s `QpResult` now carries its multipliers too, which is what
the SQP iteration reads.

## Where to go next

- The full grammar of every call is on the [cheat sheet](cheatsheet.md#optimization).
- The design decisions — why elimination rather than a linear solver, why projected BFGS rather
  than L-BFGS-B — are summarised in the [architecture](architecture.md) page.
