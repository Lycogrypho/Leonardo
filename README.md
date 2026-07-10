# Leonardo

## Introduction

Leonardo is a Scala 3 symbolic math library and Computer Algebra System (CAS). The name is an homage to [Leonardo Pisano](https://en.wikipedia.org/wiki/Fibonacci), commonly known as Fibonacci, the Italian mathematician who introduced Arabic numerals and mathematical notation to the Western world.

This project is loosely inspired by the Scala project [Cascala/Galileo]([https://github.com/cascala/galileo|), though the codebase has been completely rewritten from scratch.

## Overview

Leonardo is a lightweight CAS designed to parse, represent, and evaluate mathematical expressions. It builds an Abstract Syntax Tree (AST) from textual input and can evaluate expressions both numerically and symbolically.

### Main Characteristics

- **Expression Parsing**: Parses mathematical expressions using a recursive descent parser based on `scala-parser-combinators`. Supports standard operators, mathematical functions (`sin`, `cos`, `tan`/`tg`, `asin`, `acos`, `atan`, `exp`, `log`), implicit multiplication, unary minus in any operand position (`3 * -x`, `2^-x`), multi-character variable names (`theta`, `x1`, `alpha`), and built-in constants `pi` and `e`.

- **Dual Evaluation**: Expressions evaluate to either:
  - A numeric result (`Double`) if all variables are bound
  - A symbolic result (an AST node) if variables remain unbound

- **Variable Binding**: Support for binding variables to numeric values, allowing mixed symbolic-numeric evaluation of complex expressions. Multi-character variable names are fully supported.

- **Rich AST Representation**: Expressions are represented as a type-safe AST with nodes for:
  - Numbers and variables
  - Binary operations (addition, subtraction, multiplication, division)
  - Unary functions (exponential, logarithm, trigonometric)
  - Power operations
  - Higher-order operators (derivatives, definite integrals via Simpson's rule, and indefinite integrals via a symbolic rule table)

- **Matrix Domain** (API-only, no parser syntax yet): matrices are grids of arbitrary expressions — numbers, variables, functions, even functionals — evaluated element-wise. When every element reduces to a number, the matrix collapses to a dense row-major `Array[Double]` value (`_MatrixValue`), on which sum, product (row-parallel above a work threshold), transpose, and scalar multiplication run as array kernels; otherwise operations combine element-wise symbolically and stay symbolic until the free variables are bound. The calculus and structural algorithms (`derive`, `integrate`, `simplify`, `expand`) distribute element-wise over matrices, matrix sums, and transposes (d/dx [aᵢⱼ] = [daᵢⱼ/dx]); matrix products deliberately stay symbolic under differentiation, as they need the product rule.

- **Precision Control**: Configurable decimal precision for numeric results, with rational approximation semantics.

- **Clean API**: Environment-aware evaluation with no implicit global state. Expressions are immutable and composable. `Environment` is immutable — `withBinding` returns a new instance, enabling safe concurrent evaluation.

- **Performance**: Rounding is deferred to display time only (no mid-computation precision loss). Every AST node caches its free-variable set (`freeVars`) after the first traversal, making `dependsOn` O(1). Definite-integral evaluation (Simpson's rule) uses a compiled `Double => Double` closure when the integrand has no unresolvable symbolic nodes, eliminating per-step allocations.

## Interactive CLI

A REPL ships alongside the library. Launch it with the `sbt repl` alias (or the full
`sbt "runMain it.grypho.scala.leonardo.cli.repl"`):

```
leonardo> x = 3.001            -- bind a value (constant right-hand side)
leonardo> f = sin(x) + x       -- define a function (free variables ⇒ definition)
leonardo> f                    -- evaluate against current bindings
3.14113
leonardo> derive(f, x)         -- differentiation works through definitions
0.00987
leonardo> simplify x + 0       -- structural simplification (ignores bindings)
x
leonardo> precision 8          -- set decimal precision
leonardo> env                  -- list precision, bindings, definitions
leonardo> :save session.txt    -- write current state to a replayable script
leonardo> :load session.txt    -- replay a session script
leonardo> quit
```

Definitions are late-bound: redefining `f` also changes any `g` defined in terms
of `f`. Whether an assignment binds a value or defines a function is decided by the
right-hand side alone — constant expressions fold to a numeric binding, expressions
with free variables become definitions. Differentiating *with respect to a defined
function* applies the chain rule: with `f = sin(x)` and `g = f^2`, `derive(g, f)`
computes dg/df as `derive(g, x) / derive(f, x)` over the definition's single free
variable (definitions with several free variables are rejected with a message). `:save` serializes the session (precision,
bindings, definitions) as a script that `:load` replays.

## Planned Features

- Broader indefinite integration (integration by parts, non-linear substitution)
- Expression normalization (combining like terms across sub-trees)
- Additional mathematical functions and constants
- Parser and REPL syntax for matrix literals and operations
