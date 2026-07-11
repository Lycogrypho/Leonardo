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

- **Matrix Domain**: matrix literals parse as `[[1, 2], [3, 4]]` (a row vector is `[[1, 2]]`), with `transpose(...)` and the ordinary `+`, `-`, `*`, `/` operators — `M := [[1, 2], [3, 4]]` then `M * M` works in the REPL, and `:save`d sessions restore matrix bindings. Matrices are grids of arbitrary expressions — numbers, variables, functions, even functionals — evaluated element-wise. When every element reduces to a number, the matrix collapses to a dense row-major `Array[Double]` value (`_MatrixValue`), on which sum, product (block-tiled for cache locality, parallel over row blocks above a work threshold), transpose, and scalar multiplication run as array kernels; otherwise operations combine element-wise symbolically and stay symbolic until the free variables are bound. The calculus and structural algorithms (`derive`, `integrate`, `simplify`, `expand`) distribute element-wise over matrices, matrix sums, and transposes (d/dx [aᵢⱼ] = [daᵢⱼ/dx]); matrix products deliberately stay symbolic under differentiation, as they need the product rule.

- **Equations**: `10 * x = 2 * x + 1` parses as a relation; once all variables are bound, it evaluates to `true`/`false` using tolerance-based equality tied to the configured precision (so `sin(pi) = 0` is true despite floating-point noise). Concrete matrices compare element-wise. `derive`, `simplify`, `expand`, and `integrate` apply to both sides. `solve(lhs = rhs, x)` solves for a variable: linear equations exactly (symbolic coefficients included, `x = -b/a`), quadratics via the discriminant (0, 1, or 2 real roots, `±√Δ` closed forms when coefficients are symbolic), and transcendental or higher-degree forms numerically (sign-change scan plus bisection over [-100, 100], up to 8 roots). One solution prints as `x = 0.125`; several as `[[x = -2.0, x = 2.0]]`.

- **Precision Control**: Configurable decimal precision for numeric results, with rational approximation semantics.

- **Clean API**: Environment-aware evaluation with no implicit global state. Expressions are immutable and composable. `Environment` is immutable — `withBinding` returns a new instance, enabling safe concurrent evaluation.

- **Performance**: Rounding is deferred to display time only (no mid-computation precision loss). Every AST node caches its free-variable set (`freeVars`) after the first traversal, making `dependsOn` O(1). Definite-integral evaluation (Simpson's rule) uses a compiled `Double => Double` closure when the integrand has no unresolvable symbolic nodes, eliminating per-step allocations. `derive` and `simplify` are memoized behind bounded thread-safe caches — repeated derivatives of the same tree (e.g. Simpson's-rule fallback sampling) and `simplifyFully`'s fixpoint passes are paid once.

## Interactive CLI

A REPL ships alongside the library. Launch it with the `sbt repl` alias (or the full
`sbt "runMain it.grypho.scala.leonardo.cli.repl"`):

```
leonardo> x := 3.001           -- bind a value (constant right-hand side)
leonardo> f := sin(x) + x      -- define a function (free variables ⇒ definition)
leonardo> f                    -- evaluate against current bindings
3.14113
leonardo> derive(f, x)         -- differentiation works through definitions
0.00987
leonardo> 10 * x = 2 * x + 1   -- bare "=" is an equation: true/false once bound
false
leonardo> solve(10 * x = 2 * x + 1, x)
x = 0.125
leonardo> simplify x + 0       -- structural simplification (ignores bindings)
x
leonardo> C := A * B           -- with A, B matrices: simplify C executes the
leonardo> simplify C           -- multiplication and simplifies each element
leonardo> precision 8          -- set decimal precision
leonardo> env                  -- list precision, bindings, definitions
leonardo> :save session.txt    -- write current state to a replayable script
leonardo> :load session.txt    -- replay a session script
leonardo> quit
```

Assignment uses `:=` (the CAS convention): bare `=` always denotes an equation, so
`x = 2*x + 1` is a relation to evaluate, never a binding. Session scripts emit `:=`;
old `=`-style `:save` files are not accepted and must be re-created.

Definitions are late-bound: redefining `f` also changes any `g` defined in terms
of `f`. Whether an assignment binds a value or defines a function is decided by the
right-hand side alone — constant expressions fold to a numeric binding, expressions
with free variables become definitions. Differentiating *with respect to a defined
function* applies the chain rule: with `f := sin(x)` and `g := f^2`, `derive(g, f)`
computes dg/df as `derive(g, x) / derive(f, x)` over the definition's single free
variable (definitions with several free variables are rejected with a message). `:save` serializes the session (precision,
bindings, definitions) as a script that `:load` replays.

- **Normalization**: `normalize(e, x)` collects like terms into an ascending polynomial in one variable (`10x - 2x` → `8x`, whatever the tree shape), and `collect(e, x)` extracts the dense coefficient list — the foundation for the upcoming equation solver. Non-polynomial forms are left untouched.

## Planned Features

- Systems of linear equations (LU decomposition over the matrix domain)
- Broader indefinite integration (integration by parts, non-linear substitution)
- Additional mathematical functions and constants
