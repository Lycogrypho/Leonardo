<div style="text-align: center"><img src="docs/src/Banner.svg" alt="Leonardo" width="80%"/></div>

## Introduction

Leonardo is a Scala 3 symbolic math library and Computer Algebra System (CAS). The name is an homage to [Leonardo Pisano](https://en.wikipedia.org/wiki/Fibonacci), commonly known as Fibonacci, the Italian mathematician and author of [Liber Abbaci](https://en.wikipedia.org/wiki/Liber_Abaci#cite_note-sigler-3): with his works he introduced Indo/Arabic numerals and mathematical notation to the Western world.

This project is loosely inspired by the Scala project [Cascala/Galileo]([https://github.com/cascala/galileo|), though the codebase has been completely rewritten from scratch.

## Overview

Leonardo is a lightweight CAS designed to parse, represent, and evaluate mathematical expressions. It builds an Abstract Syntax Tree (AST) from textual input and can evaluate expressions both numerically and symbolically.

### Main Characteristics

- **Expression Parsing**: Parses mathematical expressions using a recursive descent parser based on `scala-parser-combinators`. Supports standard operators, mathematical functions (`sin`, `cos`, `tan`/`tg`, `asin`, `acos`, `atan`, `exp`, `ln`, `log`, `log(x, b)`), implicit multiplication, unary minus in any operand position (`3 * -x`, `2^-x`), multi-character variable names (`theta`, `x1`, `alpha`), and built-in constants `pi`, `e`, and `i` (imaginary unit). Logarithms: `ln(x)` is the natural log; `log(x)` is decimal (base-10); `log(x, b)` is the general base-b log computed as `ln(x)/ln(b)` — so `log(1000, 10) = 3`, `log(8, 2) = 3`.

- **Dual Evaluation**: Expressions evaluate to either:
  - A numeric result (`Double`, or a complex value) if all variables are bound
  - A symbolic result (an AST node) if variables remain unbound

- **Complex Numbers**: The imaginary unit `i` is a built-in constant (`2 + 3i`, `3i`), sitting alongside `pi` and `e`. Arithmetic is a full field — `i*i = -1`, `(2 + 3i)*(1 - i) = 5 + i`, division and powers included — implemented as a `_Complex(re, im)` value that collapses back to a plain real whenever the imaginary part vanishes, so real math is untouched. The elementary functions `exp`, `ln`, `log`, `sin`, `cos`, `tan` accept complex arguments (`exp(i*pi) = -1`, Euler's identity), and complex closure means roots and logarithms of negatives now return their principal complex values: `(-2)^0.5 = i√2`, `ln(-1) = iπ`, `(-8)^(1/3)` is the principal complex cube root. Values display as `(a + bi)` (or `bi` / `i` when purely imaginary) and round-trip through the parser; a floating-point residual imaginary part rounds away at display, so `exp(i*pi)` prints `-1.0`. Genuinely undefined forms (`ln(0)`, `0^-1`, division by zero) still stay symbolic.

- **Variable Binding**: Support for binding variables to numeric values, allowing mixed symbolic-numeric evaluation of complex expressions. Multi-character variable names are fully supported.

- **Rich AST Representation**: Expressions are represented as a type-safe AST with nodes for:
  - Numbers and variables
  - Binary operations (addition, subtraction, multiplication, division)
  - Unary functions (exponential, logarithm, trigonometric)
  - Power operations
  - Higher-order operators (derivatives, definite integrals via Simpson's rule, indefinite integrals via a symbolic rule table, and limits)

- **Matrix Domain**: matrix literals parse as `[[1, 2], [3, 4]]` (a row vector is `[[1, 2]]`), with `transpose(...)` and the ordinary `+`, `-`, `*`, `/` operators — `M := [[1, 2], [3, 4]]` then `M * M` works in the REPL, and `:save`d sessions restore matrix bindings. Matrices are grids of arbitrary expressions — numbers, variables, functions, even functionals — evaluated element-wise. When every element reduces to a number, the matrix collapses to a dense row-major `Array[Double]` value (`_MatrixValue`), on which sum, product (block-tiled for cache locality, parallel over row blocks above a work threshold), transpose, and scalar multiplication run as array kernels; otherwise operations combine element-wise symbolically and stay symbolic until the free variables are bound. The calculus and structural algorithms (`derive`, `integrate`, `simplify`, `expand`) distribute element-wise over matrices, matrix sums, and transposes (d/dx [aᵢⱼ] = [daᵢⱼ/dx]); matrix products deliberately stay symbolic under differentiation, as they need the product rule.

- **Equations**: `10 * x = 2 * x + 1` parses as a relation; once all variables are bound, it evaluates to `true`/`false` using tolerance-based equality tied to the configured precision (so `sin(pi) = 0` is true despite floating-point noise). Concrete matrices compare element-wise. `derive`, `simplify`, `expand`, and `integrate` apply to both sides. Equations are first-class values: `h := x^2 = 4` stores the relation, and `solve(h, x)` then works. `lhs == rhs` is an explicit equality check (same semantics, but not accepted by `solve()`). `solve(lhs = rhs, x)` — or equivalently `solve(h, x)` for a named equation — solves for a variable: linear equations exactly (symbolic coefficients included, `x = -b/a`), quadratics via the discriminant (0, 1, or 2 real roots, `±√Δ` closed forms when coefficients are symbolic), and transcendental or higher-degree forms numerically (sign-change scan plus bisection over [-100, 100], up to 8 roots). One solution prints as `x = 0.125`; several as `[[x = -2.0, x = 2.0]]`.

- **Limits**: `limit(expr, var, point)` computes lim_{var → point} expr. Supports two-sided and one-sided limits (`limit(1/x, x, 0, +)` → `inf`; `limit(1/x, x, 0, -)` → `-inf`). Handles indeterminate forms via L'Hôpital's rule (0/0 and ∞/∞, up to 5 steps), and limits at ±∞ for polynomial/rational functions, `exp`, `ln`, `atan`, and elementary compositions. `inf` is a built-in constant equal to `+∞`; `-inf` follows from unary minus.

- **Laplace & Fourier Transforms**: `laplace(f, t, s)` computes the Laplace transform L{f(t)} via a symbolic rule table — constants (`c → c/s`), powers (`t^n → n!/s^(n+1)`), exponentials (`e^(ct) → 1/(s−c)`), `sin(wt) → w/(s²+w²)`, `cos(wt) → s/(s²+w²)`, linearity, and the first-shift theorem `e^(at)·g(t) → G(s−a)` applied recursively — so `laplace(t*exp(-t), t, s)` yields `1/(s+1)²`. `fourier(f, t, w)` is the unilateral Fourier transform, computed as the Laplace transform evaluated at `s = i·w`; results are generally complex-valued, riding on the complex-number support (`fourier(exp(-2*t), t, w)` → `1/(2 + i·w)`). Shapes outside the table stay symbolic.

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

- **Linear system solver**: `solveSystem([[eq₁, eq₂, …]], x, y, …)` solves a square system of n linear equations in n unknowns. Coefficient extraction uses `collect` (the same polynomial prerequisite as `solve`). Dense path: Gaussian elimination with partial pivoting on Double arrays. Symbolic path: row reduction using `_Expression` arithmetic and `simplifyFully` when any coefficient or constant stays symbolic. Named equation matrices work too: `S := [[eq1, eq2]]; solveSystem(S, x, y)`. Solutions display as `[[x = 2.0, y = 1.0]]`.

## Planned Features

- Broader indefinite integration (integration by parts, non-linear substitution)
- Additional mathematical functions and constants

## Credits

Design Credits: Leonardo's Logo and banner were created using Inkscape, elaborating the following elements:

- **Rotunda Pommerania font** by Peter Wiegel — free for commercial use (available at 1001fonts.com) 
- **Fibonacci's portrait** — vectorized from "I benefattori dell'umanità" (vol. VI, Firenze: Ducci, 1850), sourced from Wikimedia Commons and used under Creative Commons license
