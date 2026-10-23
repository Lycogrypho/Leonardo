<p align="center"><img src="docs/src/Banner.svg" alt="Leonardo" width="100%"/></p>

[![CI](https://github.com/Lycogrypho/Leonardo/actions/workflows/ci.yml/badge.svg)](https://github.com/Lycogrypho/Leonardo/actions/workflows/ci.yml)
[![Maven Central](https://img.shields.io/maven-central/v/it.grypho/leonardo_3.svg?label=Maven%20Central)](https://central.sonatype.com/artifact/it.grypho/leonardo_3)
[![Docs](https://github.com/Lycogrypho/Leonardo/actions/workflows/pages.yml/badge.svg?branch=main)](https://lycogrypho.github.io/Leonardo/)
[![Scaladoc](https://img.shields.io/badge/scaladoc-API-blue.svg)](https://lycogrypho.github.io/Leonardo/api/index.html)
[![License](https://img.shields.io/badge/license-Apache%202.0-green.svg)](LICENSE)
[![Scala](https://img.shields.io/badge/scala-3.3%20LTS-red.svg)](https://www.scala-lang.org/)


## Introduction

Leonardo is a Scala 3 symbolic math library and Computer Algebra System (CAS). The name is an homage to [Leonardo Pisano](https://en.wikipedia.org/wiki/Fibonacci), commonly known as Fibonacci, the Italian mathematician and author of the [Liber Abaci](https://en.wikipedia.org/wiki/Liber_Abaci): with his works he introduced Hindu–Arabic numerals and mathematical notation to the Western world.

**Where it fits.** Scala is well served for *numerical* work — [Spire](https://github.com/typelevel/spire) for generic numerics, [Breeze](https://github.com/scalanlp/breeze) for linear algebra — but symbolic mathematics has meant reaching outside the language, to [SymPy](https://www.sympy.org/) on Python or [Symja](https://github.com/axkr/symja_android_library) on Java. Leonardo fills that slot natively: it differentiates and integrates expressions, solves them symbolically, and keeps `1/3` exactly `1/3`, with no Python runtime, no foreign binary, and no interop layer — just a dependency line.

This project was loosely inspired by the Scala project [Cascala/Galileo](https://github.com/cascala/galileo), though the codebase has been completely rewritten from scratch and Leonardo is now basically unrelated to Galileo.

**[▶ Try Leonardo in your browser](https://lycogrypho.github.io/Leonardo/app/)** — explore every feature right now: the whole CAS compiled to JavaScript, with no install, no sign-up and no server. Nothing you type leaves the tab.

## A first look

Four independent examples from an actual session. The REPL needs no project and no build, so
you can run them yourself in the browser or with the single `cs launch` command in
[Installation](#installation) below:

```
leonardo> solve(x^2 - 4 > 0, x)        -- an inequality; the answer is a union
((x < -2.0) or (x > 2.0))

leonardo> domain(ln(x - 2), x)         -- where is this expression defined?
(x > 2.0)

leonardo> curl([[-y], [x], [0]], x, y, z)   -- a vector field is just a matrix
[[0.0], [0.0], [2.0]]

leonardo> exact on                     -- switch off floating point entirely
exact = on, working precision = 30
leonardo> 0.1 + 0.2                    -- not 0.30000000000000004
3/10
leonardo> fib(100)                     -- exact well past what a Double can hold
354224848179261915075
```

**[▶ Try Leonardo in your browser](https://lycogrypho.github.io/Leonardo/app/)**

## Main characteristics

Full detail for every entry below is in the [feature reference](https://lycogrypho.github.io/Leonardo/features.html).

### Features

<ul>
<li>
<details>
<summary><i>Expression parsing</i></summary>

A recursive-descent parser over `scala-parser-combinators`, with implicit multiplication (`3sin(a)`), unary minus in any operand position, multi-character names, and the constants `pi`, `e`, `i` and `inf`.

</details>
</li>
<li>
<details>
<summary><i>Dual evaluation</i></summary>

Every expression evaluates to a number when its variables are bound and to a simplified expression tree when they are not.

This simple rule is the spine of the library: nothing throws when an answer is not yet available, it simply stays symbolic.

</details>
</li>
<li>
<details>
<summary><i>Numbers</i></summary>

- Complex numbers throughout: `i` is the imaginary unit, and a fractional power or the logarithm of a negative number is its principal complex value (`(-4)^0.5` is `2.0i`).
- Integers that remember their base: `0xFF`, `0b1011`, `0o17`, `tobase(n, b)` for any radix from 2 to 36, and balanced ternary; `A := 0xFF` still prints `0xFF` after a save and reload, while `A + 1` is plain `256`.

</details>
</li>
<li>
<details>
<summary><i>Exact arithmetic</i></summary>

When `exact` is set on, literals become exact rationals, so `0.1 + 0.2` is exactly `3/10` and `det` of the 3×3 Hilbert matrix is exactly `1/2160`.

Transcendentals are arbitrary-precision rather than `Double`, so raising the working precision genuinely sharpens `sin` and `exp`.

`exact` is off by default, because exactness comes with a performance price and for normal use the floating-point path is enough.

</details>
</li>
<li>
<details>
<summary><i>Interval arithmetic</i></summary>

`[lo, hi]` is a value with rigorous, outward-rounded bounds:

- Arithmetic, powers and the elementary functions return an interval guaranteed to contain every possible result: for example, `sin([0, pi])` finds its maximum inside the interval.
- Comparisons answer `unknown` when overlapping ranges cannot decide them, and exact endpoints stay exact.

</details>
</li>
<li>
<details>
<summary><i>Declines rather than guesses</i></summary>

An operation outside what the library can actually compute returns unevaluated instead of approximating.

A confidently wrong answer is treated as the worst possible outcome, worse than no answer at all.

</details>
</li>
<li>
<details>
<summary><i>LaTeX output</i></summary>

With `latex` set on, every result is also rendered as typeset-ready LaTeX.

The work is a precedence pass rather than a transcription: `(a+b)/(c+d)` comes out as `\frac{a + b}{c + d}` with both pairs of parentheses *gone*, and every node without a rule degrades to `\mathrm{…}`, so the emitter is plain where it is incomplete.

The printed answer is unchanged — the LaTeX version travels on a second channel, which the browser REPL typesets.

</details>
</li>
<li>
<details>
<summary><i>Clean API</i></summary>

No global state: `Environment` is immutable and `withBinding` returns a copy, so evaluation is safe to share across threads.

</details>
</li>
<li>
<details>
<summary><i>Performance</i></summary>

Rounding happens only at display time, so no precision is lost mid-computation.

Free-variable sets are cached per node, and `derive`/`simplify` are memoised behind bounded thread-safe caches.

Definite integrals compile the integrand to a `Double => Double` closure where they can, and the same compiler turns a model or a Jacobian over several variables into a closure for code that evaluates it thousands of times.

</details>
</li>
</ul>

### Domains

Each domain is a package that imports `core`, the shared foundation, while `core` imports none of them; the domains are kept as independent of each other as possible.

<ul>
<li>
<details>
<summary><i>Scalar algebra and calculus</i></summary>

Differentiation, definite integration by Simpson's rule, and indefinite integration through a layered engine:

- a rule table of ~64 verified entries
- integration by parts
- trigonometric-power reduction
- full rational partial fractions
- u-substitution
- trigonometric/hyperbolic substitution
- Weierstrass half-angle substitution.

Also the order functions: element-wise `maximum`/`minimum`, the reductions `max`/`min`, `clamp` and the smooth `softplus`, with derivatives at their kinks stated rather than guessed.

</details>
</li>
<li>
<details>
<summary><i>Matrices</i></summary>

Literals of arbitrary expressions, which collapse to dense `Double` kernels once every cell is numeric, with all the standard operations:

- determinant, inverse, trace and integer powers
- the shape `size`
- `dot`/`cross` products
- `norm` (1, 2, inf, Frobenius, vector p-norms)
- the decompositions `lu`/`qr`/`eigen`/`eig`/`jordan`
- the Cholesky factor `chol`, which refuses any matrix that is not symmetric positive definite
- the singular value decomposition `svd`, with `pinv`, `rank`, `cond` and the `nullspace`/`orth` bases
- block assembly: `hcat`, `vcat`, `blkdiag`, `kron`, `repmat`, `submatrix`.

</details>
</li>
<li>
<details>
<summary><i>Equations</i></summary>

- `solve` for a scalar or a *matrix* unknown, including the Sylvester and Lyapunov forms via Kronecker vectorization
- inequalities, whose answer is a union of intervals
- `solveSystem` for linear systems.

</details>
</li>
<li>
<details>
<summary><i>Logic</i></summary>

Boolean, three-valued Kleene, symmetric ternary and fuzzy logic share one rule table: the classical truth tables emerge as a special case of the more general ones.

</details>
</li>
<li>
<details>
<summary><i>Probability and statistics</i></summary>

- distributions as first-class values with closed-form CDFs
- a linearity rule table for `expect`/`variance`
- descriptive statistics that stay exact, medians, quantiles and sorting
- inference: t-tests, proportion, variance and chi-squared tests, ANOVA and confidence intervals
- regression by QR (ordinary, ridge and weighted) and recursive least squares with forgetting
- Bayes' theorem over a finite set of hypotheses (exact on exact input) and conjugate posteriors for the Beta–Binomial, Gamma–Poisson and Normal–Normal pairs
- seeded sampling from any distribution and maximal-length PRBS signals, reproducible across the JVM and the browser.

</details>
</li>
<li>
<details>
<summary><i>Optimization</i></summary>

- every stationary point of a function, solved symbolically and classified
- convexity proved or refuted
- Lagrange multipliers and the Karush–Kuhn–Tucker conditions
- a numeric `minimize` (gradient descent, Newton, BFGS, projected BFGS for bounds) that answers only with a certified point
- `qp`, a convex quadratic-programming solver under linear constraints — the problem a model-predictive controller solves every cycle
- `lsqfit`, nonlinear least squares that fits a model to data and answers only a certified, identifiable fit
- `lp`, exact linear programming over the rationals
- `nlp`, constrained nonlinear programming by SQP or an augmented Lagrangian, answering a certified KKT point.

</details>
</li>
<li>
<details>
<summary><i>Vector calculus</i></summary>

`grad`, `div`, `curl`, `laplacian`, `jacobian`, `hessian` over an explicit ordered coordinate tuple; the first four in Cartesian, cylindrical and both spherical conventions, `jacobian` and `hessian` in Cartesian coordinates.

</details>
</li>
<li>
<details>
<summary><i>Transforms and differential equations</i></summary>

- Laplace, Fourier (two-sided and causal), inverse Laplace and the one-sided z-transform over a symbolic rule table
- the discrete Fourier transform and convolution
- first-order initial-value problems solved in closed form where possible and by Runge–Kutta otherwise
- constant-coefficient linear systems solved by the matrix exponential
- time-varying or nonlinear systems solved by Runge–Kutta, certified by step doubling
- the integrator is also public as a step and a trajectory.

</details>
</li>
<li>
<details>
<summary><i>Control systems</i></summary>

Transfer functions are ordinary expressions rather than a carrier type, with:

- interconnection
- poles and zeros
- stability, and the Routh table
- step and impulse response
- Bode and Nyquist
- the root locus
- state space, with controllability and observability
- pole placement by Ackermann's formula
- linearisation of a nonlinear model at an operating point
- ARX identification of a black-box model from recorded data
- discrete-time simulation
- discretisation by seven named methods (holds, Tustin prewarped or not, impulse invariance, matched pole-zero, Euler)
- MPC prediction matrices
- the discrete Riccati equation with LQR
- the Kalman filter.

</details>
</li>
<li>
<details>
<summary><i>Series</i></summary>

- Taylor and Maclaurin
- numeric Fourier series
- Padé approximants
- Laurent series about a pole.

</details>
</li>
<li>
<details>
<summary><i>Special functions and sequences</i></summary>

- the factorial and gamma family with `erf`, `digamma` and the incomplete gamma and beta
- Fibonacci and its relatives over one shared recurrence, plus a generic `tabulate`.

</details>
</li>
<li>
<details>
<summary><i>Domain analysis</i></summary>

`domain`, `differentiable` and `singularities` report where an expression is defined, describing what the library computes rather than what is mathematically true.

</details>
</li>
</ul>


## Installation

Leonardo is published to Maven Central for **Scala 3.3 LTS**. The library is what you almost
always want:

```scala
libraryDependencies += "it.grypho" %% "leonardo" % "3.8.2"
```

The interactive REPL is a second artifact: a standalone program that drives Leonardo from the
command line. It depends on `leonardo`, so this line replaces the one above rather than
joining it:

```scala
libraryDependencies += "it.grypho" %% "leonardo-repl" % "3.8.2"
```

To simply *try* the REPL, nothing needs to be cloned, built or added to a project — with
[coursier](https://get-coursier.io/) installed, one command fetches it and starts a session:

```
cs launch it.grypho:leonardo-repl_3:3.8.2 -M it.grypho.scala.leonardo.cli.repl
```

### First steps in code

The REPL is a convenience; the library is the deliverable. Parse a string into an expression
tree, transform it symbolically, and evaluate it only when you choose to:

```scala
import it.grypho.scala.leonardo.core.*
import it.grypho.scala.leonardo.scalar.*
import it.grypho.scala.leonardo.parser.Parser

val f  = Parser.parse("x^3 + 2*x").get          // ((x ^ 3.0) + (2.0 * x))
val df = simplify(derive(f, _Variable("x")))    // ((3.0 * (x ^ 2.0)) + 2.0)

df.eval(new Environment(5, Map("x" -> _Number(2.0))))
// Right(14.0)      -- bound: a value

df.eval(new Environment())
// Left(((3.0 * (x ^ 2.0)) + 2.0))   -- unbound: the reduced expression, not an error

integrate(Parser.parse("sin(x)^2").get, _Variable("x"))
// (((-1.0 * (sin(x) * cos(x))) / 2.0) + (0.5 * x))
```

That `Either` is the whole design: `Right` when every variable is bound, `Left` carrying the
most-reduced form when they are not. Nothing throws because an answer is not yet available.

See [Getting Started](https://lycogrypho.github.io/Leonardo/getting-started.html) for the
import conventions and a fuller tour.

## Documentation

📖 **[Documentation site](https://lycogrypho.github.io/Leonardo/)** — guides with executed examples, two series of lessons, and the full Scaladoc API reference:

- [Getting Started](https://lycogrypho.github.io/Leonardo/getting-started.html)
- [Features](https://lycogrypho.github.io/Leonardo/features.html)
- [Cheat Sheet](https://lycogrypho.github.io/Leonardo/cheatsheet.html)
- [REPL Guide](https://lycogrypho.github.io/Leonardo/repl.html)
- Lessons [at the prompt](https://lycogrypho.github.io/Leonardo/lessons/repl.html) and [in your program](https://lycogrypho.github.io/Leonardo/lessons/library.html)
- [Architecture](https://lycogrypho.github.io/Leonardo/architecture.html)
- [Developer Guide](https://lycogrypho.github.io/Leonardo/developer.html)
- [Scaladoc API](https://lycogrypho.github.io/Leonardo/api/index.html)

## Interactive REPL

The same command language runs in three places: the
[browser app](https://lycogrypho.github.io/Leonardo/app/), which adds plotting (`plot`, `points`,
`bode`, `nyquist`, `rlocus`, `hist`) and a math editor; the `cs launch` line above; and, from a
clone, the `sbt repl` alias.

```
leonardo> x := 3.001           -- bind a value (constant right-hand side)
leonardo> f := sin(x) + x      -- define a function (free variables ⇒ definition)
leonardo> f                    -- evaluate against current bindings
3.14113
leonardo> derive(f, x)         -- differentiation works through definitions
0.00987
leonardo> 10 * x = 2 * x + 1   -- bare "=" is an equation: true/false once bound
false
leonardo> 10 * x == 2 * x + 1  -- "==" is an equality check: same eval, not solvable
false
leonardo> h := x^2 = 4         -- bind a named equation
leonardo> solve(h, x)           -- pass a named equation to solve
[[x = -2.0, x = 2.0]]
leonardo> solve(10 * x = 2 * x + 1, x)  -- inline equation still works
x = 0.125
leonardo> limit(sin(x)/x, x, 0) -- L'Hôpital: 1.0
leonardo> limit(1/x, x, 0, +)  -- one-sided: inf
leonardo> limit(atan(x), x, inf) -- limit at ∞: π/2
leonardo> laplace(sin(2*t), t, s) -- Laplace transform: 2/(s² + 4)
leonardo> fourier(exp(-abs(t)), t, w) -- Fourier transform: 2/(1 + w²)
leonardo> invlaplace(2/(s^2+4), s, t) -- inverse Laplace: sin(2*t)
leonardo> ode(y, y, t, 0, 1, 1)   -- solve y'=y, y(0)=1 at t=1: 2.71828 (e)
leonardo> simplify x + 0       -- structural simplification (ignores bindings)
x
leonardo> C := A * B           -- with A, B matrices: simplify C executes the
leonardo> simplify C           -- multiplication and simplifies each element
leonardo> g := consolidate(f + f)  -- freeze the simplified+evaluated result (not late-bound)
leonardo> precision 8          -- set decimal precision
leonardo> env                  -- list precision, bindings, definitions
leonardo> :save session.txt    -- write current state to a replayable script
leonardo> :load session.txt    -- replay a session script
leonardo> quit
```

Three rules are worth knowing before the first session:

- **`:=` assigns, and bare `=` is always an equation**, so `x = 2*x + 1` is a relation to
  evaluate, never a binding.
- **A right-hand side with free variables is a late-bound definition**: redefining `f` changes
  every `g` defined in terms of it. `consolidate(expr)` freezes a result instead.
- **`:save` writes the session** (precision, bindings, definitions) as a script that `:load`
  replays; in the browser both use the browser's own storage.

The [REPL Guide](https://lycogrypho.github.io/Leonardo/repl.html) covers every command, the
display settings (`pretty`, `latex`, `colors`), line editing and history in the terminal, and
differentiating with respect to a definition.

## Planned Features

Candidate domains under consideration. They are placeholders rather than commitments,
listed roughly by effort-to-value:

- **Number theory** — primes, `primepi`, continued fractions, modular arithmetic and the
  classical integer functions; the cheapest of the open domains and already half-specified.
- **Queueing theory** — the half of operational research still open; its other half, exact
  linear programming (`lp`), has shipped, and general MILP solving stays outside the library.
- **Quaternions** — a fifth sibling of `_Number` on the established value pattern, with
  non-commutative multiplication as the design risk to be handled deliberately.
- **Tensor algebra** — generalising the matrix domain to N dimensions, with contraction and
  Einstein summation.


## Licence

Leonardo is licensed under the **[Apache License, Version 2.0](LICENSE)**.

```
Copyright 2023-2026 Cosimo Attanasi

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0
```

You may use, modify and redistribute Leonardo — including in closed-source and commercial
work — provided you keep the licence and copyright notices and state what you changed. The
licence also grants an express patent licence from every contributor. See [`NOTICE`](NOTICE)
for the attribution notice you must carry when redistributing, and for the third-party
dependency inventory (all permissive: Apache-2.0, BSD and MIT).

Leonardo was released under GPL-3 until 2026-09-06; the relicence was made by the sole
copyright holder to let the library be used from projects under any licence.

## Credits

Leonardo's logo and banner were created with Inkscape from the following elements:

- **Rotunda Pommerania font** by Peter Wiegel — free for commercial use (available at 1001fonts.com)
- **Fibonacci's portrait** — vectorized from "I benefattori dell'umanità" (vol. VI, Firenze: Ducci, 1850), sourced from Wikimedia Commons and used under Creative Commons license
