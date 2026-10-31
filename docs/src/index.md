---
title: Leonardo
nav_order: 1
---

<p style="text-align:center"><img src="Banner.svg" alt="Leonardo" style="max-width:100%;width:100%"/></p>

Leonardo is a Scala 3 symbolic mathematics library and Computer Algebra System (CAS).
It parses mathematical expressions into an immutable AST and evaluates them either
numerically — when all variables are bound — or symbolically, returning a simplified
expression tree.

Named after [Leonardo Pisano](https://en.wikipedia.org/wiki/Fibonacci), known as Fibonacci, the author of the [Liber Abaci](https://en.wikipedia.org/wiki/Liber_Abaci).

## Try it in your browser

**[Open the browser REPL](https://lycogrypho.github.io/Leonardo/app/){: .btn .btn-primary }**

The whole system compiled to JavaScript — no install, no sign-up, no server. Differentiate,
integrate, solve, invert a matrix, take a Laplace transform, or plot a function. Sessions are
saved in your own browser and a link carries its session in the URL fragment, so nothing you
type ever leaves the tab.

## Quick start

```scala mdoc:silent
import it.grypho.scala.leonardo.core.*
import it.grypho.scala.leonardo.scalar.*
import it.grypho.scala.leonardo.parser.Parser
```

Parse an expression and differentiate it symbolically:

```scala mdoc
val expr = Parser.parse("x^3 + 2*x").get
val d    = derive(expr, _Variable("x"))
simplify(d).toString
```

Evaluate it numerically at `x = 2`:

```scala mdoc
val env = new Environment(5, Map("x" -> _Number(2.0)))
d.eval(env)
```

## Features

| Domain | Capability |
|--------|------------|
| **Parsing** | Recursive descent; implicit multiplication, multi-character names, right-associative `^` |
| **Algebra** | `+` `-` `*` `/` `^`; `sin cos tan asin acos atan exp log`; `pi` `e` `i` |
| **Calculus** | Symbolic differentiation, indefinite integration (rule table), definite integration (Simpson's rule) |
| **Simplification** | Single-pass structural reduction; fixpoint `simplifyFully` |
| **Matrices** | Symbolic `_Matrix` + dense `_MatrixValue`; arithmetic, `det`, `inv`, the decompositions (`lu`, `qr`, `eig`, `jordan`, `chol`, `svd`), block matrices, `norm`, `dot`, `cross`, `trace`, `rank`, `pinv` |
| **Equations** | `_Equation` relation; `solve` (linear exact, quadratic, numeric bisection, inequalities, matrix unknowns); `solveSystem`, with more equations than unknowns solved when consistent |
| **Complex** | `_Complex(re, im)`; full field arithmetic; `exp log sin cos tan` on complex args; principal roots |
| **Transforms** | Laplace, the two-sided and causal Fourier transforms, inverse Laplace, the one-sided z-transform and its inverse, `dft`/`idft`/`conv` |
| **Series** | Taylor and Maclaurin, numeric Fourier series, Padé approximants, Laurent series about a pole |
| **Logic** | Boolean, three-valued (Kleene), symmetric ternary, and fuzzy — one shared rule table |
| **Probability & statistics** | Distributions as first-class values; `expect`/`variance` by linearity; Bayes' theorem and conjugate posteriors; seeded sampling and PRBS; descriptive and order statistics, regression by QR (ridge and weighted too), recursive least squares, t, proportion, variance and independence tests, ANOVA |
| **Optimization** | Stationary points solved and classified; convexity; Lagrange and KKT conditions; certified numeric `minimize`, `qp`, `lsqfit` and `nlp`; exact `lp` |
| **Vector calculus** | `grad div curl laplacian` in Cartesian, cylindrical, and spherical coordinates; `jacobian hessian` in Cartesian |
| **Control** | Transfer-function algebra, poles and stability, the Routh table, step/impulse response, Bode/Nyquist, the root locus, state space, controllability and pole placement, discretisation, linearisation, ARX identification, MPC prediction matrices, LQR and the Kalman filter |
| **ODEs** | Initial-value problems: closed forms where possible, linear systems by the matrix exponential, certified Runge–Kutta for nonlinear systems |
| **Exact arithmetic** | Opt-in rational tier, square roots kept as exact surds (`√13`), arbitrary-precision transcendentals and a user-settable working precision; `rationalize` brings a decimal back |
| **Intervals** | `[lo, hi]` values with rigorous, outward-rounded bounds through arithmetic and the elementary functions; a comparison of overlapping intervals answers `unknown` |
| **Sampling** | `sample(e, v, lo, hi, n)` → `Vector[(Double, Double)]`; compiled `Double ⇒ Double` fast path |
| **REPL** | Interactive session with bindings, named functions, session scripts |

## New here? Start with a lesson

Twenty-one short lessons, each ending in an exercise, and every example on them is executed when this
site is built. Two branches, by what you are holding:

- **[Lessons: at the prompt](lessons/repl.md)** — the REPL, in a terminal or in your browser.
  Nothing to install; [R1](lessons/r1.md) opens in the browser REPL.
- **[Lessons: in your program](lessons/library.md)** — Leonardo as a library in your own Scala.
  Starts at [L1](lessons/l1.md) with a dependency line.

The pages below are the *reference*: they answer "what does this do?", where a lesson asks
"how do I get started?".

## Pages

- [Getting Started](getting-started.md) — add to your project, first expressions
- [Features](features.md) — the complete feature reference
- [Expressions & Evaluation](expressions.md) — the AST and dual eval model
- [Calculus](calculus.md) — differentiation, integration, sampling
- [Matrices](matrix.md) — the matrix domain
- [Equations](equations.md) — relations, solver, complex numbers
- [Logic](logic.md) — boolean, three-valued, symmetric ternary, and fuzzy logic
- [Sequences](sequences.md) — Fibonacci and friends, and the generic tabulator
- [Control Systems](control.md) — transfer functions, stability, response, discretisation
- [Optimization](optimization.md) — stationary points, convexity, constrained and numeric optimisation
- [Interactive REPL](repl.md) — session commands and scripts
- [Browser REPL](https://lycogrypho.github.io/Leonardo/app/) — the same REPL, in your browser
- [Cheat sheet](cheatsheet.md) — every REPL command on one page
- [Architecture](architecture.md) — package diagram and design decisions
- [Developer Guide](developer.md) — onboarding reference for contributors
- [Lessons: at the prompt](lessons/repl.md) · [Lessons: in your program](lessons/library.md) —
  the guided path through both of the above

## Licence

Leonardo is licensed under the
[Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0) —
Copyright 2023-2026 Cosimo Attanasi. Use, modification and redistribution are permitted,
including in closed-source and commercial work, provided the licence and copyright notices
are kept and changes are stated.
