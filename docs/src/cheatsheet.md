---
title: Cheatsheet
nav_order: 13
---

<img src="logo_bw.svg" alt="" style="height:80px;width:auto;float:right;margin:0 0 8px 16px"/>

# Leonardo Cheatsheet
<div style="clear:both"></div>

Quick reference for the interactive REPL and all mathematical expressions.
For full detail on any command run `help <command>` at the REPL prompt.

---

## REPL session commands

| Command | Description |
|---|---|
| `help` / `?` | Full command listing |
| `help <cmd>` / `? <cmd>` | Detail for one command |
| `env` / `vars` | Show all bindings and definitions |
| `precision <n>` | Set display/comparison precision (default 5) |
| `colors dark\|light\|none` | Syntax-highlight scheme (default `dark`) |
| `pretty on\|off` | Multi-line matrix display, echo included (default `off`); turns `latex` off |
| `latex on\|off` | Also render each result as LaTeX (default `off`); the printed text is unchanged, and the browser REPL typesets it *beside* the text. Turns `pretty` off — a typeset matrix already carries the layout |
| `names on\|off` | Announce each newly-seen free variable (default `off`). A name used as a function it is not — `sqrt(x)` — is reported whatever this is set to |
| `exact on\|off` | Exact rational arithmetic (default `off`) |
| `exact precision <n>` | Digits an irrational is approximated to (default 30, 1 to 1000) |
| `simplify <expr>` | Structural simplification; also reduces any derivative, integral, limit or transform in the expression. Definitions are expanded, numeric bindings are not |
| `expand <expr>` | Distribute products over sums; same preparation as `simplify` |
| `eval <expr>` | Evaluate substituting current bindings |
| `samples <expr> <v> <lo> <hi> [n]` | Sample function on a grid (default 200 pts, max 100 000) |
| `truth <expr>` | Truth table over the expression's free variables |
| `truth3 <expr>` | Three-valued (Kleene) truth table: false / unknown / true |
| `logic symmetric on\|off` | Spell truth values as -1 / 0 / 1 (default: off) |
| `logic minmax\|product\|lukasiewicz` | Fuzzy t-norm family (default: minmax) |
| `unset <name>` | Remove a binding or definition |
| `:save <file>` | Write session to a replayable script (browser: its own storage) |
| `:load <file>` | Replay a session script (browser: its own storage) |
| `quit` / `exit` | Leave the REPL |
| `plot <expr> <v> <lo> <hi> [n]` | **Browser only** — draw `y = f(x)` as a line |
| `points <expr> <v> <lo> <hi> [n]` | **Browser only** — draw the samples as points, axes locked to the same scale |
| `bode <expr> <v> <wMin> <wMax> [n]` | **Browser only** — Bode diagram: dB and unwrapped degrees, log frequency axis (`wMin > 0`) |
| `nyquist <expr> <v> <wMin> <wMax> [n]` | **Browser only** — Nyquist diagram in the complex plane, axes locked |

---

## Assignment

```
x := 3.14              numeric binding (constant RHS)
f := sin(x) + x        symbolic definition (late-bound: follows redefinitions)
h := 2*x = x + 1      named equation (pass to solve)
g := consolidate(f)    freeze current value of f into g (not late-bound)
L, U, P := lu(A)       tuple binding from a 1×n decomposition result
```

`=` is always an equation relation, never assignment. Use `:=` for binding.

---

## Expression syntax

| Syntax | Meaning |
|---|---|
| `+` `-` `*` `/` | Arithmetic |
| `^` | Power (right-associative: `2^3^2` = `2^9`) |
| `-x` | Unary minus |
| `3sin(x)` / `3x` | Implicit multiplication |
| `(expr)` | Grouping |

### Built-in constants

| Token | Value |
|---|---|
| `pi` | π ≈ 3.14159 |
| `e` | Euler's number ≈ 2.71828 |
| `i` | Imaginary unit (√−1) |
| `inf` | +∞ |
| `-inf` | −∞ |

---

## Mathematical functions

| Expression | Meaning |
|---|---|
| `abs(x)` | Absolute value; on a complex value, the modulus (`abs(3 + 4i)` is `5`) |
| `exp(x)` | eˣ |
| `ln(x)` | Natural logarithm |
| `log(x)` | log₁₀(x) |
| `log(x, b)` | log_b(x) |
| `sin(x)` | Sine |
| `cos(x)` | Cosine |
| `tan(x)` / `tg(x)` | Tangent |
| `asin(x)` | Arcsine |
| `acos(x)` | Arccosine |
| `atan(x)` | Arctangent |
| `sinh(x)` / `cosh(x)` / `tanh(x)` | Hyperbolic sine / cosine / tangent |
| `asinh(x)` / `acosh(x)` / `atanh(x)` | Inverse hyperbolic functions |
| `sec(x)` / `csc(x)` / `cot(x)` | Secant / cosecant / cotangent (`1/cos`, `1/sin`, `cos/sin`) |
| `sech(x)` / `csch(x)` / `coth(x)` | Hyperbolic secant / cosecant / cotangent |
| `step(x)` | Heaviside unit step (1 if x ≥ 0, 0 otherwise) |
| `maximum(a, b, …)` / `minimum(a, b, …)` | The larger / smaller argument — **element-wise** on matrices, a number spreading over every cell |
| `max(v)` / `min(v)` | The largest / smallest **entry** of one array, over all entries (shape irrelevant) |
| `clamp(x, lo, hi)` | `x` limited to `[lo, hi]`, element-wise; bounds may be numbers or columns |
| `softplus(x, k)` | `ln(1 + e^(k·x))/k`, the smooth `maximum(0, x)`; `k > 0` |

Two names, two operations: `maximum` **joins** its arguments, `max` **reduces** one array. Both
keep an exact value exact (they select, they do not compute) and stay unevaluated on a complex or
truth value and on a name with no value yet. At a tie the **first** argument wins, which is what
their derivatives use: `derive(maximum(x, 2*x), x)` is `1` at `x = 0`. `differentiable` excludes
the kinks — `differentiable(maximum(0, x), x)` and `differentiable(step(x), x)` are both `x ≠ 0`.

`simplify` folds the ratio spelling into the reciprocal node, so there is one form of each:
`1/cos(x)` → `sec(x)`, `cos(x)/sin(x)` → `cot(x)`, `1/cosh(x)` → `sech(x)`, `1/cos(x)^2` → `sec(x)^2`.

---

## Numeric sequences

```
fib(10)                 -> 55       standard indexing: fib(0)=0, fib(1)=1 (OEIS A000045)
fib(n, 1, 1)                        the classic rabbit pair = fib(n+1)
fib(5, 1.5, -pi)                    any numeric seeds -- not just integers
lucas(6)                -> 18       = fib(n, 2, 1)
pell(5)                 -> 29       x(n) = 2x(n-1) + x(n-2)
jacobsthal(5)           -> 11       x(n) = x(n-1) + 2x(n-2)
binom(10, 5)            -> 252      generalised: binom(-1, 3) = -1
catalan(5)              -> 42
harmonic(4)             -> 2.0833   exact mode gives 25/12
```

In exact mode Fibonacci is computed over arbitrary-precision integers: a `Double` stops being
exact at `fib(78)`, so `fib(100)` is `354224848179261915075` rather than an approximation.

### tabulate — a row of terms

```
tabulate(fib(k), k, 0, 8)      -> [[0, 1, 1, 2, 3, 5, 8, 13, 21]]
tabulate(binom(4, k), k, 0, 4) -> [[1, 4, 6, 4, 1]]        a Pascal row
tabulate(k^2, k, 1, 5)         -> [[1, 4, 9, 16, 25]]
at(tabulate(fib(k), k, 0, 10), 1, 11) -> 55                rows are 1-based
```

`tabulate` works for *any* expression, not just the sequences; the result is an ordinary
1×n matrix, so `at`, tuple assignment and `:save` all apply.  `k` is a binder.

### sum / product — the terms folded

```
sum(k, k, 1, 100)              -> 5050.0
sum(k^2, k, 1, 4)              -> 30.0
product(k, k, 1, 5)            -> 120.0
sum(binom(4, k), k, 0, 4)      -> 16.0                    a Pascal row, added up
sum(k*x, k, 1, 3)              -> x + 2x + 3x             symbolic terms EXPAND
sum(k, k, 1, 0)                -> 0.0                     the empty sum; product gives 1
```

The same shape as `tabulate`, which yields the *terms* where these yield their total.  An
exact term stays exact — `sum(1/k, k, 1, 3)` is `11/6` under `exact on`.  A free bound stays
symbolic, and the range is capped like `tabulate`'s so a mistyped bound declines.

---

## Special functions

```
fact(5)                 -> 120.0    exact for integers up to 170!; 171! stays symbolic
fact(0.5)               -> 0.88623  analytic continuation, = Gamma(1.5) = sqrt(pi)/2
dfact(7)                -> 105.0    double factorial 7!! = 7*5*3*1
mfact(10, 3)            -> 280.0    multifactorial, step 3: 10*7*4*1
Gamma(5)                -> 24.0     Gamma(n) = (n-1)!
Gamma(0.5)              -> 1.77245  = sqrt(pi)
lgamma(1e5)                         ln|Gamma| -- finite where Gamma overflows
Beta(1, 4)              -> 0.25     Gamma(a)Gamma(b)/Gamma(a+b)
Γ(5)                -> 24.0     Greek alias; type it with ALT-\ g or ALT-G
β(1, 4)                -> 0.25     Greek alias; type it with ALT-\ b
Si(1)                   -> 0.94608  sine integral (also Ci, Ei, li)
fresnelS(1)             -> 0.43826  Fresnel integrals (also fresnelC)
```

`Gamma` and `Beta` are **capitalised** so that lowercase `gamma` and `beta` remain
usable as ordinary variable names.  The Greek aliases Γ and β parse to the same
nodes; `toString` always emits the ASCII spelling, so `:save` scripts stay portable.
Capital Greek Beta is not accepted: it is a homoglyph of Latin `B`.  Poles (0, -1, -2, ...), overflow and complex
arguments all stay symbolic rather than returning infinities.

### Analytic tier

```
erf(x)   erfc(x)                error function and its complement
digamma(z)                      logarithmic derivative of Gamma; makes Gamma/fact differentiable
gammaP(a, x)   gammaQ(a, x)     regularised incomplete gamma (lower / upper)
betaI(x, a, b)                  regularised incomplete beta
```

---

## Probability & statistics

### Distributions — first-class values

```
normal(m, s)   uniform(a, b)   exponential(l)     continuous families
binomial(n, p) poisson(l)                         discrete families
studentt(nu)   chisq(k)                           Student-t and chi-squared
betadist(a, b) gammadist(k, rate)                 Beta and Gamma; gammadist is SHAPE-RATE,
                                                  not shape-scale (its mean is k/rate)
pdf(d, x)      cdf(d, x)                          density / mass and cumulative distribution
prob(d, lo, hi)   quantile(d, p)                  interval probability and inverse cdf
prob(X < 2)                                       predicate form; `and` intersects,
                                                  `or` is not supported
expect(d)      variance(d)                        a distribution's own moments
expect(e, X)   variance(e, X)                     moments of an expression, by linearity:
                                                  expect(2*X + 3, X) -> 2*E[X] + 3
```

### Bayesian inference

```
bayes(prior, likelihood)             Bayes' theorem over a finite set of hypotheses: two
                                     rows (or columns) in, the normalised posterior out,
                                     in the same shape; exact on exact input
posterior(prior, lik, data)          conjugate update; the likelihood is the family with
                                     its unknown parameter left FREE, and the result is an
                                     ordinary distribution (pdf/cdf/quantile/expect apply)
  posterior(betadist(a, b), binomial(n, p), k)          -> betadist(a + k, b + n - k)
  posterior(gammadist(k, r), poisson(l), [[x1, x2]])    -> gammadist(k + sum x, r + n)
  posterior(normal(m0, s0), normal(m, s), [[x1, x2]])   -> normal(mn, sn), s KNOWN
                                     anything outside the table stays symbolic
```

### Descriptive statistics and inference

```
mean(x)  variance(x)  stddev(x)      sample or distribution; sample variance is
                                     the unbiased n-1 form
pvariance(x)  pstddev(x)             population forms, dividing by n
covariance(x, y)  correlation(x, y)  two-sample statistics
regress(X, y)                        least squares by QR; no intercept column added
rls(theta, P, phi, y, lambda)        one recursive-least-squares update -> [[theta', P']]
                                     theta n x m, P n x n, phi n x 1, y m x 1 (or a number),
                                     lambda in (0, 1]; use  theta, P := rls(theta, P, ...)
ttest(sample, mu)                    two-sided one-sample t-test p-value
confint(sample, level)               confidence interval for the mean, as [[lo, hi]]
chisqtest(obs, exp)                  Pearson goodness-of-fit p-value
```

---
## Optimization

An answer that cannot be proved complete is not given: the call stays as written.

```
stationary(f, x, y)                  every point where grad f = 0, one row per point;
                                     false when provably none; the REPL classifies
                                     each as minimum / maximum / saddle point
convex(f, x, y)                      true (proved from every PRINCIPAL minor of the
                                     Hessian), false (with a witness), else symbolic
lagrange(f, [[g1], [g2]], x, y)      stationary points of f on g1 = 0, g2 = 0;
                                     a cell may be an equation, one constraint needs
                                     no matrix
kkt(f, g, h, x, y)                   the KKT conditions of: minimise f, g <= 0, h = 0
                                     (multipliers mu0.., lambda0..); 0 = absent group
minimize(f, [[x], [y]], x0, m)       numeric minimiser as a column; m is gd, newton,
                                     bfgs or pbfgs, always named
minimize(f, [[x], [y]], x0, lb, ub, m)   with bounds (inf for none): pbfgs or gd
                                     a point is returned only if its gradient passes
                                     the precision's tolerance; else the REPL says why
```

---
## Calculus

### Differentiation

```
derive(sin(x), x)               -> cos(x)
derive(x^3, x)                  -> (3.0 * (x ^ 2.0))
derive(f, x, y)                 mixed partial ∂²f/∂x∂y
derive(f, x, x)                 second derivative d²f/dx²
```

### Indefinite integration

```
integral(x^2, x)                -> (1/3) * x^3
integral(exp(x), x)             -> exp(x)
integral(sin(x), x)             -> -cos(x)
integral(1/x, x)                -> ln(x)
integral(step(x), x)            -> x*step(x)
integral(tan(x), x)             -> -ln(cos(x))            from the data-driven table
integral(k^x, x)                -> k^x / ln(k)            symbolic base free of x
integral(1/(a^2 + x^2), x)      -> atan(x/a) / a          symbolic parameter a
integral(x * exp(x^2), x)       -> exp(x^2)/2             non-linear u-substitution (u = x²)
integral(sin(x)^3 * cos(x), x)  -> sin(x)^4/4             u = sin(x)
integral(tan(x)^3, x)           -> tan²/2 + ln(cos x)     tan/sec/csc/cot power reduction
integral(1/((x-1)^2*(x-2)), x)                            full partial fractions, repeated root
integral(1/((x^2+1)*(x-1)), x)                            partial fractions, complex pair
integral(1/(4 - x^2)^0.5, x)    -> asin(x/2)              trigonometric substitution
integral((4 - x^2)^1.5, x)                                any half-integer power of the radicand
integral(1/(x^2 + 1)^0.5, x)    -> asinh(x)               hyperbolic substitution
integral(1/(2 + cos(x)), x)                               Weierstrass t = tan(x/2)
integral(sin(x)/x, x)           -> Si(x)                  special integral functions
integral(exp(-x^2), x)          -> sqrt(pi)/2 * erf(x)
integral(1/ln(x), x)            -> li(x)
integral(asin(x), x)            -> x*asin(x) + (1-x^2)^0.5 from the data-driven table
integral(sin(2*x)*cos(5*x), x)                            product-to-sum
integral(1/(a^2 - x^2), x)      -> atanh(x/a)/a           symbolic parameter a
integral(1/(x^2 + a^2)^0.5, x)  -> asinh(x/a)
integral(x^a, x)                -> x^(a+1)/(a+1)          symbolic exponent
integral(exp(a*x)*sin(b*x), x)                            general cyclic pair
```

### Vector calculus

A vector field is an n×1 matrix; the coordinate tuple is explicit and **ordered**.

```
grad(x^2*y, x, y)               -> [[2xy], [x^2]]         gradient (n x 1)
div([[x^2], [y^3]], x, y)       -> 2x + 3y^2              divergence (scalar)
curl([[-y], [x], [0]], x, y, z) -> [[0], [0], [2]]        curl (3-D only)
laplacian(x^3*y, x, y)          -> 6xy                    = div(grad(f))
jacobian([[x*y], [y^2]], x, y)  -> [[y, x], [0, 2y]]      m x n
hessian(x^2*y^3, x, y)                                    n x n, symmetric
```

`curl` outside three dimensions, a component/coordinate count mismatch, and a repeated
coordinate all stay symbolic rather than being guessed.

An optional trailing keyword selects the coordinate system (default `cartesian`):

```
laplacian(1/r, r, t, p, spherical)        -> 0        the Newtonian potential is harmonic
div([[1/r], [0], [0]], r, t, z, cylindrical) -> 0     the 2-D point source is source-free
grad(f, r, t, z, cylindrical)             -> [[df/dr], [(1/r)*df/dt], [df/dz]]
```

`cylindrical` is `(r, θ, z)`; `spherical` is `(r, θ, φ)` with **θ the polar angle** (the
physics convention) and `sphericalmaths` is `(r, θ, φ)` with **θ azimuthal and φ polar** (the
mathematics one).  All are three-dimensional, and the coordinates are identified by
**position, not by name** — call them whatever you like, but pass them in that order.  The two
spherical keywords therefore differ in argument *order*, not in naming: pass the polar angle
second for `spherical`, third for `sphericalmaths`.

### Definite integration (Simpson's rule)

The limits are ordinary **expressions**, so arithmetic, constants and other variables are all
legal there.

```
integral(sin(x), x, 0, pi)      -> ≈ 2.0
integral(sin(x), x, 0, 2*pi)    -> ≈ 0.0        over a full period
integral(sin(x), x, -100*pi, 100*pi)          an expression bound is not a special case
integral(integral(1, y, 0, 2*x), x, 0, 1) -> 1.0   iterated, inner limit in the outer variable
```

### Limits

```
limit(sin(x)/x, x, 0)           -> 1.0
limit(1/x, x, 0, +)             -> inf    (from right)
limit(1/x, x, 0, -)             -> -inf   (from left)
limit(atan(x), x, inf)          -> 1.5708 (π/2)
limit((1 + 1/x)^x, x, inf)     -> 2.71828 (e)
```

### Sampling

```
samples sin(x) x -pi pi
samples f x 0 10 500            500 points for defined function f
```

---

## Series expansions

```
maclaurin(exp(x), x, 4)     -> 1 + x + x^2/2 + x^3/6 + x^4/24
maclaurin(sin(x), x, 7)     odd powers only
maclaurin(cos(x), x, 6)     even powers only
maclaurin(1/(1-x), x, 5)    the geometric series
taylor(exp(x), x, 1, 4)     expanded about x = 1
taylor(x^2, x, a, 2)        symbolic centre: a polynomial in (x - a)
fourierSeries(x, x, 2*pi, 4)      sawtooth: 2sin(x) - sin(2x) + (2/3)sin(3x) - ...
fourierSeries(x^2, x, 2*pi, 4)    even function: cosine terms only, a0/2 = pi^2/3
fourierSeries(sin(pi*x), x, 2, 3) period 2, so omega = pi
pade(exp(x), x, 1, 1)             -> (2 + x)/(2 - x)   rational [m/n] approximant
pade(exp(x), x, 2, 2)             beats maclaurin(exp(x), x, 4) away from 0
pade(1/(1+x), x, 0, 1)            a rational function reproduces itself exactly
laurent(1/(x*(x-1)), x, 0, 3)     Laurent series about a pole; the principal-part
laurent(sin(x)/x^2, x, 0, 1, 3)   length is optional (detected via singularities)
```

`fourierSeries` expands over one period centred on 0, with coefficients computed
numerically by Simpson — so the period must be a concrete positive number.  It is a
different operation from `fourier(e, t, w)`, the Fourier *transform*.  Coefficients are
not chopped: a term that vanishes analytically returns at the integrator noise floor.

`maclaurin(e, v, n)` is sugar for `taylor(e, v, 0, n)`.  The expansion variable stays
free in the result, so binding it evaluates the polynomial.  Order is capped at 20;
an expression whose derivative stays symbolic (`Gamma`, `fact`) returns unevaluated
rather than a bogus series.

---
## Equations & solving

### Equation relations

```
2*x + 1 = 5                     symbolic equation (evaluates to _Bool when x is bound)
x^2 + 1 == x^2 + 1              equality check (not solvable via solve)
```

### solve — single equation

```
solve(2*x + 1 = 5, x)           -> x = 2.0  (linear)
solve(x^2 = 4, x)               -> [[x = -2.0, x = 2.0]]  (quadratic)
solve(sin(x) = 0, x)            numeric bisection fallback
```

### solve — inequalities

The answer is written in the language itself — a comparison, or a union/intersection of
comparisons via `or`/`and`:

```
solve(x^2 - 4 > 0, x)           -> ((x < -2.0) or (x > 2.0))
solve(2*x + 1 <= 5, x)          -> (x <= 2.0)
solve(2*x > 2 and x < 5, x)     -> ((x > 1.0) and (x < 5.0))
solve(a*x > b, x)               stays symbolic: the sign of `a` is unknown, and dividing
                                by a negative coefficient would flip the relation
```

### Comparisons

```
x < 2                           (also > <= >= !=) reduce to true/false when bound,
x > 0 and x < 10                and compose with and/or/not/implies
```

### solve — matrix equation

```
solve(A * X = B, X)             -> X = inv(A) * B
solve(X * A = B, X)             -> X = B * inv(A)
solve(A * X * D = B, X)         -> X = inv(A) * B * inv(D)
solve(A * X + C = B, X)         affine: X = inv(A) * (B - C)
solve(A * X + X * B = C, X)     Sylvester equation (Kronecker vectorization)
```

### solveSystem — linear systems

```
solveSystem([[2*x + y = 3, x - y = 0]], x, y)   -> [[x = 1.0, y = 1.0]]
```

---

## Domain analysis

Where an expression is defined, differentiable, or singular — answered in the language, as
comparisons and connectives.  The analysis describes what the *library* computes, not the
full analytic extension.

```
domain(ln(x - 2), x)            -> (x > 2.0)
domain(asin(x), x)              -> ((x >= -1.0) and (x <= 1.0))
domain(1/x, x)                  -> (x != 0.0)
differentiable(ln(x), x)        -> (x > 0.0)        where the derivative exists
singularities(1/(x-1)^2, x)     -> [[1.0], [2.0]]   location over pole order
singularities(x^2, x)           -> false            provably none
domain(tan(x), x)               stays symbolic: the exclusion set is infinite
```

---

## Base conversion

The base lives **on the value**: `A := 0xFF` still prints `FF` after a `:save`/`:load`
round-trip, while remaining usable as `255` everywhere.

```
0b1011                          -> 1011   binary literal (= 11)
0o17                            -> 17     octal (= 15)
0xff                            -> FF     hexadecimal (= 255)
0t1TT                           -> 1TT    balanced ternary, digits {1, 0, T = -1} (= 5)
tobase(255, 16)                 -> FF     any radix 2..36
balanced(5)                     -> 1TT    balanced ternary conversion
0xFF + 1                        -> 256    arithmetic is decimal: the tag is display-only
```

---

## Logic

### Connectives

```
true and false                  -> false   (true/false are literals)
not a                           negation
a implies b                     material implication (right-associative)
a xor b                         exclusive or
a or b and c                    = a or (b and c)
x = 1 and y = 2                 connectives bind looser than "="
false and (1/0 = 0)             -> false   (short-circuit: right side never evaluated)
```

Precedence (tightest to loosest): `not` > `and` > `xor` > `or` > `implies`.

### Three-valued (Kleene) logic

```
unknown                         the third truth value (a bindable value like true/false)
not unknown                     -> unknown   (0.5 is the negation fixpoint)
false and unknown               -> false     (0 annihilates min)
true or unknown                 -> true      (1 annihilates max)
unknown and unknown             -> unknown
unknown implies unknown         -> unknown
```

### Symmetric ternary (an encoding, not a semantics)

```
logic symmetric on              spell truth values as -1 / 0 / 1
logic symmetric off             false / unknown / true (default)
logic symmetric                 show the current setting
-1 and 0                        -> -1   (false and unknown, with the toggle on)
1 or 0                          -> 1
0 and 0                         -> 0
not 0                           -> 0
2 * 3 + 1                       -> 7.0  (arithmetic is never reinterpreted)
1 and 0                         stays symbolic while the toggle is OFF
```

Related by `t = (s + 1) / 2`; the rule table is identical either way.
`:save` persists the toggle but always writes the word spelling.
### Fuzzy logic ([0, 1] degrees)

```
truth(0.3)                      a graded degree (also how one prints, so it round-trips)
truth(0.3) and truth(0.7)       -> truth(0.3)   (min-max)
not truth(0.3)                  -> truth(0.7)
very(0.5)                       -> truth(0.25)  concentration: d^2
somewhat(0.25)                  -> unknown      dilation: sqrt(d) = 0.5
trimf(x, a, b, c)               triangular membership curve
trapmf(x, a, b, c, d)           trapezoidal
gaussmf(x, mean, sigma)         gaussian
sigmf(x, a, c)                  sigmoid
defuzz(trimf(x,0,5,10), x, 0, 10)  -> 5.0   crisp value by centre of gravity
```

### Custom membership curves

The four built-in shapes are a convenience, not a limit: `truth(<expr>)` turns ANY
scalar expression of one variable into a membership curve.

```
truth(1 / (1 + (x - 5)^2))      a Cauchy bell -- a curve of your own
bell := truth(1 / (1 + (x-5)^2))   name it, then use the name
defuzz(bell, x, 0, 10)          -> 5.0    custom curves defuzzify like built-ins
very(bell)                      and compose with hedges and connectives
truth(2)                        stays symbolic: a degree must lie in [0, 1]
defuzz(1/(1+(x-5)^2), x, 0, 10) a bare scalar curve works too (no wrapper needed)
defuzz(trimf(x,0,3,6) or trimf(x,4,7,10), x, 0, 10)
                                aggregate curves with the connectives, then defuzzify
```

### t-norm families

```
logic minmax                    and = min, or = max (default; the only lattice)
logic product                   and = a*b, or = a+b-a*b
logic lukasiewicz               and = max(0,a+b-1), or = min(1,a+b)
logic                           show both logic settings
```

All three agree with classical logic on true/false, so the boolean and ternary tiers
are unaffected.  Idempotence and absorption hold for graded degrees under minmax only.
### Simplification & truth tables

```
simplify a and true             -> a
simplify not not a              -> a
simplify a or (a and b)         -> a   (absorption)
simplify unknown and not unknown  -> unknown   (complement is gated on crisp operands)
truth a and b                   two-valued table over the free variables (max 16)
truth3 a and not a              three-valued table: false / unknown / true (max 10)
```
## Complex numbers

`i` is the imaginary unit; arithmetic with `i` produces complex results automatically.

```
(2 + 3i) * (1 - i)             -> (5.0 + 1.0i)
exp(i * pi)                     -> -1.0  (Euler's formula)
(-1)^0.5                        -> (0.0 + 1.0i)  (principal value)
ln(-1)                          -> (0.0 + 3.14159i)
```

---

## Matrix domain

### Literals

```
[[1, 2], [3, 4]]                2×2 matrix
[[1, 2, 3]]                     1×3 row vector
```

### Constructors

```
eye(n)                          n×n identity matrix
zeros(r, c)                     r×c zero matrix
zeros(n)                        n×n zero matrix
ones(r, c) / ones(n)            all-ones matrix
```

### Block matrices — a number counts as a 1×1 block in hcat/vcat/blkdiag/kron

```
hcat(A, B, …)                   side by side (equal heights)
vcat(A, B, …)                   stacked (equal widths)
blkdiag(A, B, …)                on the diagonal, zeros elsewhere
repmat(A, m, n)                 A tiled m down, n across
kron(A, B)                      Kronecker product
submatrix(A, r0, r1, c0, c1)    rows r0..r1, cols c0..c1 — 1-BASED, INCLUSIVE (like at)

kron(eye(2), [[1, 2]])          -> [[1, 2, 0, 0], [0, 0, 1, 2]]
submatrix(A, i, i, j, j)        -> the 1×1 matrix holding at(A, i, j)
```

A free name is never guessed to be a scalar: `hcat(x, A)` waits until `x` is bound. A result
above 1 000 000 cells is refused before it is built. `sub` is **not** reserved.

### Operations

```
A + B                           element-wise sum
A - B                           element-wise difference
A * B                           matrix product (or scalar multiple if one is a number)
k * A                           scalar multiple
transpose(A)                    transpose
det(A)                          determinant (scalar)
inv(A)                          inverse
A ^ n                           integer matrix power (binary exponentiation)
pow(A, n)                       same as A^n
```

### Decompositions — result is a 1×n row, use `at(result, 1, k)` to index

```
lu(A)                           -> [[L, U, P]]  (P·A = L·U, partial pivoting)
qr(A)                           -> [[Q, R]]     (A = Q·R, modified Gram-Schmidt)
eigen(A)                        -> [[λ₁, …, λₙ]]  eigenvalues only
eig(A)                          -> [[V, D]]     A·V = V·D  (eigenvectors + diagonal)
jordan(A)                       -> [[P, J]]     A = P·J·P⁻¹
```

### Matrix exponential — a single matrix, not a row

```
expm(A)                         -> e^A          scaling + squaring, degree-13 Padé
expm(zeros(3, 3))               -> eye(3)
expm([[0, 1], [0, 0]])          -> [[1, 1], [0, 1]]   nilpotent: series terminates
expm([[0, -t], [t, 0]])         -> rotation by t
```

`expm(A)` is **not** `A^n`: that is repeated multiplication, this is the exponential series.
Works for **defective** matrices (a repeated eigenvalue with too few eigenvectors), where the
eigen-decomposition route has no basis to use.

### Cholesky factor — a single matrix, not a row

```
chol(A)                         -> L, lower triangular, A = L·Lᵀ
chol([[4, 2], [2, 3]])          -> [[2, 0], [1, 1.41421]]
chol([[1, 2], [2, 1]])          -> stays chol(...)   indefinite
```

Stays unevaluated unless `A` is square, symmetric and positive definite — the refusal *is* the
convexity test. Tolerances are relative to the largest entry; exact input stays exact.

### Indexing decomposition results

```
L, U, P := lu(A)                tuple binding
at(lu(A), 1, 1)                 first element (L)
at(eigen(A), 1, 2)              second eigenvalue
```

### Scalar functions over matrices (element-wise)

```
sin([[pi/2, 0], [0, pi]])       element-wise sin
exp([[1, 0], [0, 1]])           element-wise exp
```

---

## Integral transforms

### Laplace transform  `L{e(t)}`

```
laplace(1, t, s)                -> 1/s
laplace(t^2, t, s)              -> 2/s^3
laplace(sin(3*t), t, s)         -> 3/(s^2+9)
laplace(exp(2*t)*cos(t), t, s)  -> (s-2)/((s-2)^2+1)  first-shift
laplace(t^3*exp(-t), t, s)      derivative-of-transform rule
```

### Fourier transform  `F{e(t)} = L{e(t)}|_{s=iω}`

```
fourier(exp(-2*t), t, w)        -> 1/(2 + i*w)
fourier(1, t, w)                -> 1/(i*w)
```

### Inverse Laplace transform  `L⁻¹{f(s)}`

```
invlaplace(1/s, s, t)           -> 1
invlaplace(1/s^2, s, t)         -> t
invlaplace(1/(s-3), s, t)       -> exp(3*t)
invlaplace(2/(s^2+4), s, t)     -> sin(2*t)
invlaplace(3/((s-2)^2+9), s, t) -> exp(2*t)*sin(3*t)
```

### z-transform  `X(z) = Σ(n≥0) x[n]·z⁻ⁿ`

**One-sided**, like `laplace`. Argument order is `(sequence, index, z)`.

```
ztrans(1, n, z)                 -> z/(z-1)      constant SEQUENCE, not 1/z
ztrans(2^n, n, z)               -> z/(z-2)
ztrans(n, n, z)                 -> z/(z-1)^2
ztrans(n^2, n, z)               -> z*(z+1)/(z-1)^3
ztrans(n*2^n, n, z)             -> 2*z/(z-2)^2
ztrans(sin(0.5*n), n, z)        -> z*sin(0.5)/(z^2-2*z*cos(0.5)+1)

invztrans(z/(z-2), z, n)        -> 2^n
invztrans(z/((z-1)*(z-2)), z, n) -> 2^n - 1
invztrans(z/(z-2)^2, z, n)      -> n*2^(n-1)
```

`Z{c} = c·z/(z−1)`, **not** `c/z` — the Laplace `L{c} = c/s` does not carry over, because a
constant *sequence* is `c` at every index. Complex poles, the Kronecker delta and the bilateral
transform are not supported and stay symbolic.

---

## Control systems

A transfer function is **an ordinary expression** — no carrier type — so the frequency
variable is always an explicit argument. Grammar forms first, then the library API.

```
series(g, h, s)                 G·H, normalised to one rational in lowest terms
parallel(g, h, s)               G + H
feedback(g, h, s)               G/(1 + G·H)   NEGATIVE feedback; negate h for positive
step(g, s, t)                   step response  = invlaplace(G/s, s, t)
impulse(g, s, t)                impulse response = invlaplace(G, s, t)
```

`step` is arity-overloaded: `step(x)` is the Heaviside unit step, `step(G, s, t)` the response.

```
feedback(1/(s*(s+2)), 1, s)     -> 1/(s^2 + 2*s + 1)     common factor cancelled
step(1/(s+1), s, t)             -> 1 - exp(-t)
impulse(2/(s+3), s, t)          -> 2*exp(-3*t)
```

Library API (`import it.grypho.scala.leonardo.control.*`):

```
poles(g, s)      Option[Vector[_Value]]   denominator roots, in lowest terms
zeros(g, s)      Option[Vector[_Value]]   numerator roots      (_Complex when oscillatory)
dcgain(g, s)     Option[Double]           G(0)
isStable(g, s)   Option[Boolean]          every pole strictly in the LEFT HALF-PLANE
isStableDiscrete(g, z)                    every pole strictly INSIDE THE UNIT CIRCLE
routhTable(g, s) Option[Vector[Vector[Double]]]   jagged rows; no grammar form, so
                                          `routh` stays a legal variable name
bode(g, s, w)    Option[(magnitude, phase-radians)]     one frequency
nyquist(g, s, w) Option[(real, imaginary)]               one frequency

frequencyResponse(g, s, wMin, wMax, n, env)   Vector[(omega, dB, degrees)]
                                          GEOMETRIC grid, phase UNWRAPPED past -180
                                          wMin > 0: log(0) is not a number
nyquistSweep(g, s, wMin, wMax, n, env)    Vector[(real, imaginary)]   the same grid

stateSpace(a, b, c, d)          1x4 _Matrix of matrices, the lu/qr/eig shape
mpcMatrices(A, B, C, Np, Nc)    [[Phi, Gamma]]  Y = Phi*x + Gamma*U; last move HELD past Nc
                                (also at the prompt; a number is a 1x1 matrix)
controllable(a, b) / observable(a, c)     Option[Boolean]
c2dExact(a, b, ts)              (A_d, B_d) via expm of the block matrix; singular A is fine
c2d(g, s, z, ts, Zoh | Tustin)  Option[_Expression]   method is never a hidden default
d2c(g, z, s, ts, Tustin)        Option[_Expression]   Zoh is not invertible in closed form
```

Marginal stability is **not** stability: poles on the imaginary axis give `false`. A
non-rational `G` (a dead-time `exp(-2*s)`), an improper `G` in a time response, and an
undeterminable coefficient sign are all **declined**, never approximated.

---

## Ordinary differential equations

First-order IVP  `y' = f(t, y)`,  `y(t₀) = y₀`,  returns `y(target)`.

```
ode(k*y, y, t, 0, 1, 1)        y' = k·y, y(0)=1  -> e^k  (symbolic in k)
ode(-y, y, t, 0, 1, 1)         y' = -y,  y(0)=1  -> 0.36788 (= 1/e)
ode(-y+t, y, t, 0, 1, 1)       y' = -y+t, y(0)=1 -> integrating factor (closed form)
ode(y*y, y, t, 0, 1, 0.5)      nonlinear -> RK4 numeric fallback
```

Closed-form tier covers linear `y' = a(t)·y + b(t)`.
RK4 (4th-order Runge-Kutta) is the numeric fallback for all other shapes.

Linear **systems** `y' = A·y + b` with a column `y0` (`A` constant and square, `b` a column):

```
ode([[0, 1], [-1, 0]] * y, y, t, 0, [[1], [0]], 1)    -> [[cos 1], [-sin 1]]
A := [[0, 1], [0, 0]]                                  (a bound A works the same)
ode(A*y + [[0], [1]], y, t, 0, [[0], [0]], 2)          -> [[2.0], [2.0]]   singular A
```

`e^(A·τ)·y₀` by the augmented matrix exponential (the `c2dExact` kernel), so a defective or
singular `A` is fine. Numeric only; a non-conforming shape stays symbolic.

**Time-varying and nonlinear systems** are integrated numerically. The state is one name bound
to the whole column, or a **column of names**, each bound to its component:

```
ode([[v], [-x]], [[x], [v]], t, 0, [[1], [0]], 1)      -> [[0.5403], [-0.84147]]
ode([[x - x*y], [x*y - y]], [[x], [y]], t, 0, [[2], [1]], 5)    Lotka-Volterra
ode([[t, 0], [0, -t]] * y, y, t, 0, [[1], [2]], 1)     A(t) through the single name
```

RK4, **certified by step doubling**: `n` and `2n` steps must agree within the `=` tolerance
(`0.5·10^-precision`), or the call stays symbolic and the REPL says why — a stiff system is
refused rather than answered wrong. A right-hand side that is not a conforming column is never
broadcast.

The public step and trajectory, with the method **always named** (`euler`, `rk4`, `rk45`):

```
odeStep(F, y, t, t0, y0, h, method)        state at t0 + h      (n x 1)
odeSolve(F, y, t, t0, y0, t1, h, method)   one row per time:  [t, state...]

odeStep([[y]], [[y]], t, 0, [[1]], 0.1, euler)            -> [[1.1]]
odeSolve([[1]], [[x]], t, 0, [[0]], 1, 0.5, rk4)          -> [[0.0, 0.0], [0.5, 0.5], [1.0, 1.0]]
```

`euler`/`rk4` take exactly one step of size `h` per interval, **uncertified** — the step is
yours. `rk45` is adaptive Dormand–Prince: `h` is the spacing of the output rows, and the steps
between them hold a local error three orders below the `=` tolerance. `t1` is always the last row,
the final interval cut short when `h` does not divide the span; at most 100 000 rows. A name
bound in the session (a control input) is a constant during the step. `euler`, `rk4` and `rk45`
are matched only in the method slot, so they stay legal variable names.
