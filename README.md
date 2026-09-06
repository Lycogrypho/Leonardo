<div style="text-align: center"><img src="docs/src/Banner.svg" alt="Leonardo" width="100%"/></div>

## Introduction

Leonardo is a Scala 3 symbolic math library and Computer Algebra System (CAS). The name is an homage to [Leonardo Pisano](https://en.wikipedia.org/wiki/Fibonacci), commonly known as Fibonacci, the Italian mathematician and author of [Liber Abbaci](https://en.wikipedia.org/wiki/Liber_Abaci#cite_note-sigler-3): with his works he introduced Indo/Arabic numerals and mathematical notation to the Western world.

This project is loosely inspired by the Scala project [Cascala/Galileo]([https://github.com/cascala/galileo|), though the codebase has been completely rewritten from scratch.

📖 **Documentation site**: [lycogrypho.github.io/Leonardo](https://lycogrypho.github.io/Leonardo/) — guides, examples, and the full Scaladoc API reference (under `/api`), built from `docs/` and published to GitHub Pages by [`.github/workflows/pages.yml`](.github/workflows/pages.yml). See [`docs/README.md`](docs/README.md) for the build details.

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
  - Unary functions (exponential, logarithm, trigonometric, hyperbolic and reciprocal-trigonometric — `sinh`/`cosh`/`tanh`, `asinh`/`acosh`/`atanh`, `sec`/`csc`/`cot`, `sech`/`csch`/`coth`, with `simplify` normalising the ratio spelling into a single reciprocal node: `1/cos(x)` → `sec(x)`, `cos(x)/sin(x)` → `cot(x)`)
  - Power operations
  - Higher-order operators (derivatives, definite integrals via Simpson's rule, indefinite integrals via a symbolic rule table with integration by parts, trig-power reduction formulas (including `tan`/`sec`/`csc`/`cot` powers), full rational-function partial fractions (repeated and complex roots at any degree, via square-free factorisation), **non-linear u-substitution** for `f(g(x))·g'(x)` forms — `∫ x·e^(x²)`, `∫ sin³x·cos x` — that the linear chain rule cannot reach, **trigonometric/hyperbolic substitution** for radicals `√(a²−x²)` / `√(a²+x²)` / `√(x²−a²)` at any half-integer power (`(4−x²)^(3/2)` included), the **Weierstrass half-angle substitution** `t = tan(x/2)` for rational functions of `sin`/`cos` (`∫ 1/(1+sin x)`, `∫ 1/(2+cos x)`), the **special integral functions** `Si`/`Ci`/`Ei`/`li`/`fresnelS`/`fresnelC` as named answers to the classic non-elementary integrals (`∫ sin x/x = Si(x)`, `∫ e^(−x²) = (√π/2)·erf(x)`), and a data-driven table of ~64 entries transcribed from the standard references — `tan`, `cot`, `sec`, `csc`, the hyperbolics, the standalone inverse functions (`asin`…`atanh`), product-to-sum (`sin(ax)cos(bx)`), the general `e^(ax)sin(bx)` pair, and the symbolic-parameter algebraic/radical forms (`1/(a²−v²)` → `atanh(v/a)/a`, `1/√(v²+a²)` → `asinh(v/a)`, `v^a`, `v·a^v`) — table rules are **parameterised**, so a symbolic constant free of the integration variable is handled too (`k^v` → `k^v/ln(k)`), every entry carries the linear-argument chain rule (`tan(2x+1)` closes), and **every entry is verified by a differentiate-back harness**, and limits)

- **Matrix Domain**: matrix literals parse as `[[1, 2], [3, 4]]` (a row vector is `[[1, 2]]`), with `transpose(...)`, `det(...)`, `inv(...)`, and the ordinary `+`, `-`, `*`, `/` operators — `M := [[1, 2], [3, 4]]` then `M * M` works in the REPL, and `:save`d sessions restore matrix bindings. Matrices are grids of arbitrary expressions — numbers, variables, functions, even functionals — evaluated element-wise. When every element reduces to a number, the matrix collapses to a dense row-major `Array[Double]` value (`_MatrixValue`), on which sum, product (block-tiled for cache locality, parallel over row blocks above a work threshold), transpose, scalar multiplication, determinant (LU with partial pivoting), and inverse (Gauss–Jordan) run as array kernels; otherwise operations combine element-wise symbolically and stay symbolic until the free variables are bound. `det(A)` returns a scalar; `inv(A)` and the reciprocal spelling `1 / A` return the inverse matrix (`M / N` is `M · N⁻¹`), staying symbolic when `A` is singular or non-square. Small symbolic matrices get a cofactor determinant and adjugate/det inverse. The calculus and structural algorithms (`derive`, `integrate`, `simplify`, `expand`) distribute element-wise over matrices, matrix sums, and transposes (d/dx [aᵢⱼ] = [daᵢⱼ/dx]); matrix products deliberately stay symbolic under differentiation, as they need the product rule. Scalar functions distribute element-wise over a matrix argument too: `sin(A)`, `exp(A)`, `ln(A)`, etc. reduce to the matrix of per-element results (staying symbolic if an element leaves the function's real domain). This holds for symbolic matrices as well — `exp([[x, 1], [y, 0]])` becomes `[[exp(x), 2.71828], [exp(y), 1.0]]`, folding numeric cells and keeping free-variable cells as `f(cell)` until they are bound. A square dense matrix can be raised to an integer power with the ordinary `^` operator or `pow(A, n)`: `A^0` is the identity, `A^n` multiplies `A` by itself (binary exponentiation), and negative powers invert first (`A^-n = (A⁻¹)^n`); non-square bases, non-integer exponents, and negative powers of a singular matrix stay symbolic.

- **Equations**: `10 * x = 2 * x + 1` parses as a relation; once all variables are bound, it evaluates to `true`/`false` using tolerance-based equality tied to the configured precision (so `sin(pi) = 0` is true despite floating-point noise). Concrete matrices compare element-wise. `derive`, `simplify`, `expand`, and `integrate` apply to both sides. Equations are first-class values: `h := x^2 = 4` stores the relation, and `solve(h, x)` then works. `lhs == rhs` is an explicit equality check (same semantics, but not accepted by `solve()`). `solve(lhs = rhs, x)` — or equivalently `solve(h, x)` for a named equation — solves for a variable: linear equations exactly (symbolic coefficients included, `x = -b/a`), quadratics via the discriminant (0, 1, or 2 real roots, `±√Δ` closed forms when coefficients are symbolic), and transcendental or higher-degree forms numerically (sign-change scan plus bisection over [-100, 100], up to 8 roots). One solution prints as `x = 0.125`; several as `[[x = -2.0, x = 2.0]]`. A **matrix equation** can be solved for a scalar unknown: `solve([[x, 2*x]] = [[3, 6]], x)` decomposes into the per-cell equations, solves each, and keeps only the value that satisfies every cell (here `x = 3`); an inconsistent system such as `[[x, x]] = [[1, 2]]` has no solution and stays symbolic. The unknown may also be a **matrix**: with `A` and `B` bound, `solve(A * X = B, X)` returns the unique `X = A⁻¹·B` via the inverse kernel (singular or non-conforming `A` → no solution); `solve(X * A = B, X)` gives `X = B·A⁻¹`. Beyond the one-sided forms, a constant term is peeled to the right (`solve(A * X + C = B, X)` → `A·X = B − C`) and a matrix on each flank is inverted on both sides (`solve(A * X * D = B, X)` → `X = A⁻¹·B·D⁻¹`). Symbolic coefficients yield a symbolic matrix solution (cofactor expansion, ≤ 6×6). **General linear matrix equations** where `X` appears in several terms — the Sylvester equation `solve(A * X + X * B = C, X)`, the Lyapunov equation `solve(A * X + X * transpose(A) = C, X)`, scalar coefficients `solve(2 * X = B, X)` — are solved by Kronecker vectorization: each term `s·L·X·R` contributes `s·(Rᵀ ⊗ L)` to a dense linear system over `vec(X)`, solved via the inverse kernel and reshaped back (dense coefficients; a singular system — e.g. `A` and `−B` sharing an eigenvalue — has no solution).

- **Logic**: the word connectives `and`, `or`, `not`, `implies`, `xor` operate on the boolean literals `true`/`false` and on anything that reduces to a boolean — equations included, so `x = 1 and y = 2` is a conjunction of two relations. Connectives bind looser than `=`/`==`, with precedence `not` > `and` > `xor` > `or` > `implies` (`implies` right-associative). `and`/`or`/`implies` short-circuit on a decisive left operand (`false and X` is `false` without evaluating `X`), and expressions with free variables stay symbolic until bound. Simplification applies constant folding, double negation, idempotence, complement, and absorption; `toCNF`/`toDNF` produce conjunctive/disjunctive normal forms (De Morgan plus distribution, capped against exponential blow-up), and the REPL's `truth <expr>` command prints a truth table over an expression's free variables (up to 16).

- **Three-valued (Kleene) Logic**: the same connectives and the same min–max rule table carry a third truth value, `unknown` — the value carrier widens, the operators do not. `unknown` is a first-class value (bindable, printable, restored by `:save`) sitting at degree 0.5: `not unknown` is `unknown` (its negation fixpoint), while the crisp cases still decide (`false and unknown` is `false`, `true or unknown` is `true`) — which is exactly why the short-circuits stay valid unchanged. Crisp results collapse back to plain booleans, so the classical truth tables fall out of the graded table as a special case rather than a separate code path. The classical laws that fail at `unknown` (complement `a and not a`, `a implies a`, `a xor a`) are gated on the expression being free of graded degrees, so simplification never folds them away wrongly; `truth3 <expr>` prints the 3ⁿ table (up to 10 variables).
- **Symmetric Ternary Logic**: the same three truth values spelled with the digits `{-1, 0, 1}` instead of `false`/`unknown`/`true`, related by the affine map `t = (s + 1) / 2`. This is an *encoding*, not a second semantics: the min–max rule table is identical either way, so the Kleene identities restate verbatim (`-1 and 0` is `-1`, `1 or 0` is `1`, `not 0` is `0`). Switched per `Environment` (`logic symmetric on` at the REPL, persisted by `:save`), which means a *bound* variable participates too, not just literals. With the toggle off a bare `0` stays a plain number so nothing is silently reinterpreted, and ordinary arithmetic is untouched in both modes. Under the encoding the digit `0` counts as graded, so `0 and not 0` correctly stays `0` rather than folding to false by complement.

- **Fuzzy Logic**: the same connectives over the full `[0, 1]` interval. `truth(x)` turns a scalar degree into a truth value (and is how a graded degree prints, so it round-trips through `:save`), while membership curves — `trimf(x,a,b,c)`, `trapmf(x,a,b,c,d)`, `gaussmf(x,mean,sigma)`, `sigmf(x,a,c)` — and the hedges `very(d) = d²` / `somewhat(d) = √d` map crisp measurements into degrees. They evaluate *to* truth values, so `very(trimf(t, 0, 10, 20)) and somewhat(h)` composes with no cast at each step. Which t-norm combines degrees is an `Environment` parameter rather than a separate package: min–max (default), product (`a·b` / `a+b−a·b`), or Łukasiewicz (`max(0,a+b−1)` / `min(1,a+b)`) — all three agreeing with classical logic on the crisp values, so the boolean and three-valued tiers are untouched by the choice. Only min–max is a lattice, so idempotence and absorption hold for graded degrees there alone and simplification gates them accordingly. `defuzz(e, v, lo, hi)` collapses a membership curve back to a crisp value by centre of gravity (`meanOfMaxima` and `bisector` are available too), sampling through the same compiled-closure fast path Simpson's rule uses.

- **Special Functions**: the factorial family and the gamma function. `fact(n)` is exact for integers up to `170!` and continues analytically past them (`fact(0.5)` is `√pi/2`); `dfact(n)` is the double factorial and `mfact(n, k)` the multifactorial with step `k`. `Gamma(z)` is the Lanczos approximation with the reflection formula for negative arguments, `lgamma(z)` its log-space companion that stays finite where `Gamma` itself overflows, and `Beta(a, b)` the beta function computed through `lgamma` so large arguments survive. `Gamma` and `Beta` are capitalised deliberately, so the lowercase `gamma` and `beta` stay available as ordinary variable names. Poles, overflow and complex arguments stay symbolic rather than returning infinities, and the functions distribute element-wise over a matrix argument like the elementary ones.

- **Numeric Sequences**: `fib(n)` — the library is named after Leonardo Pisano, after all — together with `lucas`, `pell` and `jacobsthal`, all one recurrence `x(k) = p·x(k-1) + q·x(k-2)` differing only in seeds and coefficients, so each keeps its own name while the recurrence has a single definition (`lucas(n)` and `fib(n, 2, 1)` are the same number by construction). Indexing is the standard one — `fib(10) = 55`, matching OEIS A000045 — and the classic "rabbit" pair is written explicitly as `fib(n, 1, 1)`, which is the same sequence shifted by one. They are *numeric*, not integer, sequences: a recurrence needs only addition and multiplication, so `fib(5, 1.5, -pi)` is a legitimate call and exact seeds stay exact. That last point is not decoration — a `Double` stops representing Fibonacci numbers exactly at `fib(78)`, so in exact mode `fib(100)` is `354224848179261915075` rather than an approximation (Binet's formula is deliberately not used). Alongside them: `binom` (generalised to a real upper index, so `binom(-1, 3) = -1`), `catalan`, and `harmonic`, which is rational-valued and shows the exact tier off nicely — `harmonic(4)` is `25/12`. Finally `tabulate(e, k, lo, hi)` evaluates any expression over an integer range into a 1×n matrix: `tabulate(binom(4, k), k, 0, 4)` is a Pascal row, and because the result is an ordinary matrix, `at`, tuple assignment and `:save` all work on it unchanged.

- **Vector Calculus**: `grad(f, x, y, z)`, `div(F, x, y, z)`, `curl(F, x, y, z)`, `laplacian(f, x, y, z)`, plus `jacobian(F, ...)` and `hessian(f, ...)`. A vector field is simply an n×1 matrix — `div([[x^2], [y^3]], x, y)` gives `2x + 3y^2`, and `curl([[-y], [x], [0]], x, y, z)` gives `[[0], [0], [2]]` — so every existing matrix operation applies to a field unchanged. The coordinate tuple is always written out and **ordered**, never inferred, because it fixes the order of the result's components. The coordinate-free identities hold (`curl(grad f) = 0`, `div(curl F) = 0`, `laplacian = div∘grad`). Shapes with no meaning stay symbolic rather than being guessed: a `curl` outside three dimensions (in 2-D the natural object is a *scalar* curl, a different result type), a component/coordinate count mismatch, a repeated coordinate, or a row vector. **Cartesian, cylindrical and spherical** coordinates are supported through an optional trailing keyword — `laplacian(1/r, r, t, p, spherical)` is `0`, and `div([[1/r], [0], [0]], r, t, z, cylindrical)` is `0`, both because the operators are written once in terms of the coordinate system's scale factors rather than once per system. The curvilinear systems are three-dimensional and identify their coordinates **by position, not by name** (so you may call them anything). Both spherical conventions are available and differ in argument *order* rather than naming: `spherical` is the physics order `(r, θ, φ)` with `θ` the polar angle, and `sphericalmaths` the mathematics order with `θ` azimuthal and `φ` polar. Exchanging those two coordinates flips the handedness of the basis, so `curl` — being a pseudo-vector — carries the corresponding sign; the two conventions agree on the curl of the same physical field. Multivariate integration needs nothing extra — iterated integrals accept limits that depend on the outer variables, so `integral(integral(x*y, y, 0, x), x, 0, 1)` is `0.125`.

- **Series Expansions**: `taylor(e, v, point, n)` expands an expression as a truncated Taylor polynomial about a point, and `maclaurin(e, v, n)` is the special case centred on zero. The coefficients come from the memoised higher-order derivative engine, so the k-th term reuses the work of the (k−1)-th, and each is instantiated at the centre by substitution before the whole series is simplified. The centre need not be numeric: expanding about a symbolic `a` yields a genuine polynomial in `(x − a)` with `f⁽ᵏ⁾(a)` coefficients. The expansion variable stays free in the result, so binding it evaluates the polynomial and the series composes with `derive`, `samples` and the rest of the library. Order is capped at 20, and an expression whose derivative falls outside the rule table (`Gamma`, `fact` — both needing digamma) returns unevaluated rather than a series with an unresolved derivative buried in it.

- **Fourier Series**: `fourierSeries(e, v, period, n)` expands a periodic function over one period centred on zero as `a0/2 + sum of [ak*cos(kωv) + bk*sin(kωv)]`. Unlike the Taylor tier this is a *numeric* expansion: each coefficient is a definite integral evaluated by the same Simpson path used for `integral(f, x, a, b)`, so the period must be a concrete positive number. The classic results come out as expected — the sawtooth `fourierSeries(x, x, 2*pi, 4)` gives `2sin(x) - sin(2x) + (2/3)sin(3x) - (1/2)sin(4x)`, and `x^2` gives a cosine-only series with constant term `pi²/3`. Coefficients are deliberately not chopped: a term that vanishes analytically comes back at the integrator's noise floor rather than exactly zero, on the principle that cleanup is a display concern. Not to be confused with `fourier(e, t, w)`, the Fourier *transform*.

- **Pade Approximants**: `pade(e, v, m, n)` builds the `[m/n]` rational approximant about zero — the quotient `P/Q` with `deg P <= m`, `deg Q <= n` and `Q(0) = 1` whose Maclaurin series matches the function through order `m + n`. It is frequently far more accurate than the Taylor polynomial of the same total degree, because a rational function can model a nearby pole that no polynomial can: `pade(exp(x), x, 1, 1)` is the familiar `(2 + x)/(2 − x)`, and `pade(exp(x), x, 2, 2)` beats `maclaurin(exp(x), x, 4)` at `x = 1`. A rational function reproduces itself exactly, and `pade(f, x, m, 0)` degenerates to the Taylor polynomial. The defining linear system is solved through the dense matrix inverse in `core`; a singular system (no `[m/n]` approximant in normal form) leaves the expression symbolic rather than dividing by zero.

- **Greek Notation**: the special functions accept their Greek spellings — `Γ(z)` for `Gamma(z)` and `β(x, y)` for `Beta(x, y)` — and the REPL can insert the glyphs with an `ALT-\` prefix (`ALT-\ g`, `ALT-\ b`; `ALT-G` also works directly). A prefix was chosen over one chord per symbol because `ALT-B` is already `backward-word` in emacs-style line editing, and because a prefix extends to further symbols without re-checking for collisions. Capital Greek Beta is deliberately **not** accepted: U+0392 is a homoglyph of Latin `B`, so it would be invisible which of the two had been typed; the visually distinct lowercase β is used instead. Both spellings parse to the same node and `toString` always emits the ASCII form, so saved scripts stay portable to terminals that cannot render or type these.

- **Limits**: `limit(expr, var, point)` computes lim_{var → point} expr. Supports two-sided and one-sided limits (`limit(1/x, x, 0, +)` → `inf`; `limit(1/x, x, 0, -)` → `-inf`). Handles indeterminate forms via L'Hôpital's rule (0/0 and ∞/∞, up to 5 steps), and limits at ±∞ for polynomial/rational functions, `exp`, `ln`, `atan`, and elementary compositions. `inf` is a built-in constant equal to `+∞`; `-inf` follows from unary minus.

- **Laplace & Fourier Transforms**: `laplace(f, t, s)` computes the Laplace transform L{f(t)} via a symbolic rule table — constants (`c → c/s`), powers (`t^n → n!/s^(n+1)`, capped at n ≤ 20), exponentials (`e^(ct) → 1/(s−c)`), `sin(wt) → w/(s²+w²)`, `cos(wt) → s/(s²+w²)`, linearity, and the first-shift theorem `e^(at)·g(t) → G(s−a)` applied recursively — so `laplace(t*exp(-t), t, s)` yields `1/(s+1)²`. `fourier(f, t, w)` is the unilateral Fourier transform, computed as the Laplace transform evaluated at `s = i·w`; results are generally complex-valued, riding on the complex-number support (`fourier(exp(-2*t), t, w)` → `1/(2 + i·w)`). Shapes outside the table stay symbolic.

- **Inverse Laplace Transform**: `invlaplace(F, s, t)` recovers f(t) from a rational F(s) = N(s)/D(s) with deg N < deg D ≤ 2 — the dual of the forward table. Linearity peels sums and constant factors; the pole structure is read off by completing the square: linear denominators give `b/(s−a) → b·e^(at)`, distinct real roots split via partial fractions into `A·e^(r₁t) + B·e^(r₂t)`, a repeated root gives `e^(at)·(N₁ + (N₀+N₁a)·t)`, and a complex-conjugate pair gives `e^(at)·(N₁cos(wt) + …sin(wt))`. So `invlaplace(3/((s-2)^2+9), s, t)` returns `e^(2t)·sin(3t)`, and `invlaplace(laplace(f, t, s), s, t)` round-trips f. Denominators of **degree ≥ 3 are decomposed by square-free factorisation and undetermined coefficients**, so repeated poles invert too — `1/s^3 → t²/2`, `1/((s-1)^2(s-2))`, `1/(s^2+1)^2` — while a repeated irreducible quadratic past the second power, symbolic coefficients, and non-rational input stay symbolic.

- **Differential Equations**: `ode(rhs, y, t, t0, y0, target)` solves the first-order initial-value problem `y' = rhs(t, y)`, `y(t₀) = y₀`, and returns the solution value `y(target)`. Linear equations `y' = a(t)·y + b(t)` are solved in closed form: constant coefficients directly (`y' = y, y(0)=1` gives `e` exactly at `t = 1`; `y' = 2y + 3` gives the affine-plus-exponential form), and variable coefficients via the integrating factor `μ(t) = e^{∫p dt}` (`y' = -y + t` → `t − 1 + 2e^{-t}`; `y' = -y/(1+t)` → `1/(1+t)`), reusing the full indefinite-integration engine (so a forcing term like `t·eᵗ` closes by integration by parts). A free `target`, a free initial condition, or a symbolic coefficient yields a symbolic solution (`ode(k*y, y, t, 0, 1, 1)` → `e^k`). Non-linear shapes (`y' = sin(y)`, `y' = -y²`) and linear ones whose coefficient integral has no closed form (`y' = tan(t)·y`) are integrated numerically with a fourth-order Runge–Kutta scheme (backward integration when `target < t₀`); a non-evaluable right-hand side or a symbolic target with no closed form stays symbolic.

- **Relations and logic**: `=` builds a solvable equation, `==` an equality test, and
  `<`, `>`, `<=`, `>=`, `!=` compare — all reducing to booleans that compose with
  `and`/`or`/`not`/`implies`, so `x > 0 and x < 10` is an ordinary expression. Comparisons
  are exact when both operands are exact and tolerance-based otherwise, with the two
  agreeing so that precisely one of `<`, `==`, `>` ever holds.

- **Statistics**: Descriptive statistics over samples (`mean`, `variance`, `stddev`,
  `covariance`, `correlation`) that stay *exact* when the sample is written exactly; least
  squares via `regress(X, y)`, solved by QR rather than the normal equations so the
  condition number is never squared; and an inference tier — `ttest`, `confint`,
  `chisqtest` — so `ttest(sample, 5) < 0.05` reads the way statistics is actually written.

- **Probability**: Distributions are first-class values — `normal(0, 1)`, `binomial(10, 0.3)`,
  `poisson(4)` and friends bind to names like any other value. `pdf`, `cdf`, `prob` and
  `quantile` answer numeric questions about them, with every cumulative distribution a closed
  form rather than a numeric integral. `expect` and `variance` carry a **linearity rule
  table**, so `expect(2*X + 3, X)` is rewritten symbolically to `2·E[X] + 3` before anything
  is computed — and `expect(X*Y, X)` deliberately stays symbolic, because it needs an
  independence assumption the language cannot state.

- **Special functions**: The factorial and gamma family (`fact`, `Gamma`, `lgamma`, `Beta`,
  with `Γ`/`β` aliases), extended by the analytic tier: `erf`/`erfc`, `digamma`, the
  regularised incomplete gamma (`gammaP`/`gammaQ`) and beta (`betaI`), and `Gamma` over
  complex arguments. `digamma` is what makes `Gamma` and `fact` differentiable.
- **Precision Control**: Configurable decimal precision for numeric results, with rational approximation semantics.

- **Exact arithmetic mode** (`exact on`): numeric literals become exact rationals, so
  `0.1 + 0.2` is exactly `3/10` and `1/3 * 3` is exactly `1`. Matrices are exact too —
  `det` of the 3×3 Hilbert matrix is exactly `1/2160` where floating point reports
  `4.6E-4`, and `A * inv(A)` is exactly the identity — and `fact` loses the `170!`
  ceiling that a `Double` imposes. Transcendentals are arbitrary-precision, so a
  user-settable *working precision* genuinely sharpens `sin` and `exp` themselves:
  `exp(1000)` is an exact 435-digit value where floating point overflows to infinity, and
  the small root of `x² + 10⁸x + 1 = 0` gets steadily more accurate as you raise it. Off by
  default, so the floating-point path is unchanged.

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

- **Normalization**: `normalize(e, x)` collects like terms into an ascending polynomial in one variable (`10x - 2x` → `8x`, whatever the tree shape), and `collect(e, x)` extracts the dense coefficient list — the foundation for the upcoming equation solver. Non-polynomial forms are left untouched.

- **Linear system solver**: `solveSystem([[eq₁, eq₂, …]], x, y, …)` solves a square system of n linear equations in n unknowns. Coefficient extraction uses `collect` (the same polynomial prerequisite as `solve`). Dense path: Gaussian elimination with partial pivoting on Double arrays. Symbolic path: row reduction using `_Expression` arithmetic and `simplifyFully` when any coefficient or constant stays symbolic. Named equation matrices work too: `S := [[eq1, eq2]]; solveSystem(S, x, y)`. Solutions display as `[[x = 2.0, y = 1.0]]`.

## Planned Features

- Additional mathematical functions and constants

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
