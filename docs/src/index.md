---
title: Leonardo
nav_order: 1
---

<p style="text-align:center"><img src="Banner.svg" alt="Leonardo" style="max-width:100%;width:100%"/></p>

Leonardo is a Scala 3 symbolic mathematics library and Computer Algebra System (CAS).
It parses mathematical expressions into an immutable AST and evaluates them either
numerically — when all variables are bound — or symbolically, returning a simplified
expression tree.

Named after [Leonardo Pisano](https://en.wikipedia.org/wiki/Fibonacci) (aka Fibonacci), the author of [Liber Abbaci](https://en.wikipedia.org/wiki/Liber_Abaci#cite_note-sigler-3).

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
| **Matrices** | Symbolic `_Matrix` + dense `_MatrixValue`; sum, product, transpose, scale, determinant, inverse (`det`, `inv`, `1/A`) |
| **Equations** | `_Equation` relation; `solve` (linear exact, quadratic, numeric bisection); `solveSystem` (Gaussian elimination) |
| **Complex** | `_Complex(re, im)`; full field arithmetic; `exp log sin cos tan` on complex args; principal roots |
| **Sampling** | `sample(e, v, lo, hi, n)` → `Vector[(Double, Double)]`; compiled `Double ⇒ Double` fast path |
| **REPL** | Interactive session with bindings, named functions, session scripts |

## Pages

- [Getting Started](getting-started.md) — add to your project, first expressions
- [Features](features.md) — the complete feature reference
- [Expressions & Evaluation](expressions.md) — the AST and dual eval model
- [Calculus](calculus.md) — differentiation, integration, sampling
- [Matrices](matrix.md) — the matrix domain
- [Equations](equations.md) — relations, solver, complex numbers
- [Sequences](sequences.md) — Fibonacci and friends, and the generic tabulator
- [Control Systems](control.md) — transfer functions, stability, response, discretisation
- [Interactive REPL](repl.md) — session commands and scripts
- [Cheat sheet](cheatsheet.md) — every REPL command on one page
- [Architecture](architecture.md) — package diagram and design decisions
- [Developer Guide](developer.md) — onboarding reference for contributors

## Licence

Leonardo is licensed under the
[Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0) —
Copyright 2023-2026 Cosimo Attanasi. Use, modification and redistribution are permitted,
including in closed-source and commercial work, provided the licence and copyright notices
are kept and changes are stated.
