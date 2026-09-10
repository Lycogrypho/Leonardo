<div style="text-align: center"><img src="docs/src/Banner.svg" alt="Leonardo" width="100%"/></div>

[![CI](https://github.com/Lycogrypho/Leonardo/actions/workflows/ci.yml/badge.svg)](https://github.com/Lycogrypho/Leonardo/actions/workflows/ci.yml)
[![Maven Central](https://img.shields.io/maven-central/v/it.grypho/leonardo_3.svg?label=Maven%20Central)](https://central.sonatype.com/artifact/it.grypho/leonardo_3)
[![Docs](https://github.com/Lycogrypho/Leonardo/actions/workflows/pages.yml/badge.svg?branch=main)](https://lycogrypho.github.io/Leonardo/)
[![Scaladoc](https://img.shields.io/badge/scaladoc-API-blue.svg)](https://lycogrypho.github.io/Leonardo/api/index.html)
[![License](https://img.shields.io/badge/license-Apache%202.0-green.svg)](LICENSE)
[![Scala](https://img.shields.io/badge/scala-3.3%20LTS-red.svg)](https://www.scala-lang.org/)

## Introduction

Leonardo is a Scala 3 symbolic math library and Computer Algebra System (CAS). The name is an homage to [Leonardo Pisano](https://en.wikipedia.org/wiki/Fibonacci), commonly known as Fibonacci, the Italian mathematician and author of [Liber Abbaci](https://en.wikipedia.org/wiki/Liber_Abaci#cite_note-sigler-3): with his works he introduced Indo/Arabic numerals and mathematical notation to the Western world.

This project was loosely inspired by the Scala project [Cascala/Galileo](https://github.com/cascala/galileo), though the codebase has been completely rewritten from scratch and Leonardo is now basically unrelated to Galileo.

## Overview

Leonardo is a lightweight CAS designed to parse, represent, and evaluate mathematical expressions. It builds an Abstract Syntax Tree (AST) from textual input and can evaluate expressions both numerically and symbolically.

### A first look

Four independent one-liners from an actual session — the REPL needs no project and no build,
so you can run these yourself with the single `cs launch` command in
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

### Main Characteristics

Full detail for every entry below is in the [feature reference](https://lycogrypho.github.io/Leonardo/features.html).

- **Expression parsing** — a recursive-descent parser over `scala-parser-combinators`, with implicit multiplication (`3sin(a)`), unary minus in any operand position, multi-character names, and the constants `pi`, `e`, `i` and `inf`.

- **Dual evaluation** — every expression evaluates to a number when its variables are bound and to a simplified expression tree when they are not. That single rule is the spine of the library: nothing throws when an answer is not yet available, it simply stays symbolic.

- **Exact arithmetic** (`exact on`) — literals become exact rationals, so `0.1 + 0.2` is exactly `3/10` and `det` of the 3×3 Hilbert matrix is exactly `1/2160`. Transcendentals are arbitrary-precision rather than `Double`, so raising the working precision genuinely sharpens `sin` and `exp`; off by default, leaving the floating-point path untouched.

- **Declines rather than guesses** — an operation outside what the library can actually compute returns unevaluated instead of approximating. A confidently wrong answer is treated as the worst possible outcome, worse than no answer at all.

- **Clean API** — no global state. `Environment` is immutable and `withBinding` returns a copy, so evaluation is safe to share across threads.

- **Performance** — rounding happens only at display time, so no precision is lost mid-computation; free-variable sets are cached per node, and `derive`/`simplify` are memoised behind bounded thread-safe caches. Definite integrals compile the integrand to a `Double => Double` closure where they can.

- **Domains** — each is a package that imports `core`, never the reverse:

  - **Scalar algebra and calculus** — differentiation, definite integration by Simpson's rule, and indefinite integration through a layered engine: a rule table of ~64 verified entries, integration by parts, trigonometric-power reduction, full rational partial fractions, u-substitution, trigonometric/hyperbolic substitution, and the Weierstrass half-angle substitution.
  - **Matrices** — literals of arbitrary expressions that collapse to dense `Double` kernels once every cell is numeric; determinant, inverse, integer powers, and the `lu`/`qr`/`eigen`/`eig`/`jordan` decompositions.
  - **Equations** — `solve` for a scalar or a *matrix* unknown (including the Sylvester and Lyapunov forms via Kronecker vectorization), inequalities whose answer is a union of intervals, and `solveSystem` for linear systems.
  - **Logic** — boolean, three-valued Kleene, symmetric ternary and fuzzy, all sharing one rule table: the classical truth tables fall out as the crisp special case rather than a separate code path.
  - **Probability and statistics** — distributions as first-class values with closed-form CDFs, a linearity rule table for `expect`/`variance`, descriptive statistics that stay exact, and regression by QR.
  - **Vector calculus** — `grad`, `div`, `curl`, `laplacian`, `jacobian`, `hessian` over an explicit ordered coordinate tuple, in Cartesian, cylindrical and both spherical conventions.
  - **Transforms and differential equations** — Laplace, Fourier, inverse Laplace and the one-sided z-transform over a symbolic rule table; first-order initial-value problems solved in closed form where possible and by Runge–Kutta otherwise.
  - **Control systems** — transfer functions as ordinary expressions rather than a carrier type: interconnection, poles and zeros, stability, step and impulse response, Bode and Nyquist, state space, and discretisation by zero-order hold or Tustin.
  - **Series** — Taylor and Maclaurin, numeric Fourier series, Padé approximants, and Laurent series about a pole.
  - **Special functions and sequences** — the factorial and gamma family with `erf`, `digamma` and the incomplete gamma and beta; Fibonacci and its relatives over one shared recurrence, plus a generic `tabulate`.
  - **Domain analysis** — `domain`, `differentiable` and `singularities` report where an expression is defined, describing what the library computes rather than what is mathematically true.

## Installation

Leonardo is published to Maven Central for **Scala 3.3 LTS**. The library is what you almost
always want:

```scala
libraryDependencies += "it.grypho" %% "leonardo" % "3.7.1"
```

The interactive REPL is a second artifact: a standalone program that drives Leonardo from the
command line. It depends on `leonardo`, so this line replaces the one above rather than
joining it:

```scala
libraryDependencies += "it.grypho" %% "leonardo-repl" % "3.7.1"
```

To simply *try* the REPL, nothing needs to be cloned, built or added to a project — with
[coursier](https://get-coursier.io/) installed, one command fetches it and starts a session:

```
cs launch it.grypho:leonardo-repl_3:3.7.1 -M it.grypho.scala.leonardo.cli.repl
```

### First steps in code

The REPL is a convenience; the library is the deliverable. Parse a string into an AST,
transform it symbolically, and evaluate it only when you choose to:

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

📖 **Documentation site**: [lycogrypho.github.io/Leonardo](https://lycogrypho.github.io/Leonardo/) — guides, examples, and the full Scaladoc API reference published to GitHub Pages. See [`docs/README.md`](docs/README.md) for the build details.

📖 **Individual pages** (published; the sources under `docs/src/` are mdoc *input* and show
their examples unevaluated):

- [Getting Started](https://lycogrypho.github.io/Leonardo/getting-started.html)
- [Features](https://lycogrypho.github.io/Leonardo/features.html)
- [Cheat Sheet](https://lycogrypho.github.io/Leonardo/cheatsheet.html)
- [REPL Guide](https://lycogrypho.github.io/Leonardo/repl.html)
- [Architecture](https://lycogrypho.github.io/Leonardo/architecture.html)
- [Developer Guide](https://lycogrypho.github.io/Leonardo/developer.html)
- [Scaladoc API](https://lycogrypho.github.io/Leonardo/api/index.html)

## Interactive CLI

From a clone, launch the REPL with the `sbt repl` alias (or the full
`sbt "replModule/runMain it.grypho.scala.leonardo.cli.repl"`); the `cs launch` line above
needs no clone at all:

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
leonardo> fourier(exp(-2*t), t, w) -- Fourier transform: 1/(2 + i·w)
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

The prompt has full line editing and history (powered by JLine): use the arrow keys to
edit the current line and to recall earlier commands, which persist across sessions in
`~/.leonardo_history`. `Ctrl-C` abandons the current line without leaving the session;
`Ctrl-D` (or `quit`/`exit`) ends it. When no interactive console is attached (piped
input, CI), the prompt degrades gracefully to a plain line reader.

Token-level syntax highlighting colours the input as you type. Three built-in schemes are
available; switch with the `colors` command:

| Command | Scheme |
|---|---|
| `colors dark` | bold yellow commands · cyan functions · magenta constants · green numbers (default) |
| `colors light` | bold blue commands · green functions · magenta constants · red numbers |
| `colors none` | no colouring |

The active scheme is persisted by `:save` and restored by `:load`.

Matrices with two or more rows can be displayed multi-line with right-aligned columns via
the `pretty on` command (off by default; `pretty off` restores the single-line
`[[…], […]]` form, `pretty` shows the current setting). The setting is likewise persisted
by `:save` / `:load`:

```
leonardo> pretty on
leonardo> [[1, 200], [30, 4]]
[[ 1.0, 200.0]
 [30.0,   4.0]]
```

Single-row matrices and decomposition results (a matrix of matrices) stay on one line.

Assignment uses `:=` (the CAS convention): bare `=` always denotes an equation, so
`x = 2*x + 1` is a relation to evaluate, never a binding. Session scripts emit `:=`;
old `=`-style `:save` files are not accepted and must be re-created.

Definitions are late-bound: redefining `f` also changes any `g` defined in terms
of `f`. Whether an assignment binds a value or defines a function is decided by the
right-hand side alone — constant expressions fold to a numeric binding, expressions
with free variables become definitions. `name := consolidate(expr)` is the opposite of a
late-bound definition: it *freezes* the result. The expression is simplified and evaluated
against the current bindings right away, and the snapshot is stored — so with `x := 2` and
`f := x + 1`, `g := consolidate(f + f)` binds `g` to `6.0` and it stays `6.0` even after
`x := 100`. If free variables remain, the frozen simplified form is kept instead (with
`a := 3`, `h := consolidate(a * y)` stores `h := (3.0 * y)`, unaffected by a later `a := 9`).
Differentiating *with respect to a defined
function* applies the chain rule: with `f := sin(x)` and `g := f^2`, `derive(g, f)`
computes dg/df as `derive(g, x) / derive(f, x)` over the definition's single free
variable (definitions with several free variables are rejected with a message). `:save` serializes the session (precision,
bindings, definitions) as a script that `:load` replays.

## Planned Features

Candidate domains, each recorded in full in the project's issue list. They are placeholders
rather than commitments, listed roughly by effort-to-value:

- **Number theory** — primes, `primepi`, continued fractions, modular arithmetic and the
  classical integer functions; the cheapest of the open domains and already half-specified.
- **Optimization** — the symbolic half only: stationary points from `grad`, their
  classification from `hessian`, and Lagrange multipliers.
- **Operational research** — adopted in part rather than whole, taking the pieces that belong
  in a CAS and leaving general MILP solving outside it.
- **Quaternions** — a fifth sibling of `_Number` on the established value pattern, with
  non-commutative multiplication as the design risk to be handled deliberately.
- **Interval arithmetic** — `_Interval(lo, hi)` as a value, answering the question the exact
  tier cannot: how far a floating-point result can be trusted.
- **Linear ODE systems** — `y' = A·y` solved in closed form as `e^(A·t)·y₀` through the
  matrix exponential that now exists.
- **Tensor algebra** — generalising the matrix domain to N dimensions, with contraction and
  Einstein summation.
- **Graph plotting and visualization** — a user-facing interface for graphing functions and
  exploring solutions; the largest single piece of work on the list.


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

Design Credits: Leonardo's Logo and banner were created using Inkscape, elaborating the following elements:

- **Rotunda Pommerania font** by Peter Wiegel — free for commercial use (available at 1001fonts.com) 
- **Fibonacci's portrait** — vectorized from "I benefattori dell'umanità" (vol. VI, Firenze: Ducci, 1850), sourced from Wikimedia Commons and used under Creative Commons license
