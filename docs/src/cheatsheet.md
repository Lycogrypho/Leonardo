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
| `pretty on\|off` | Multi-line matrix display, echo included, and answers written with the fewest brackets and a binary minus, `4 - k*k` (default `off`); `:save` is unchanged; turns `latex` off |
| `latex on\|off` | Also render each result as LaTeX (default `off`); the printed text is unchanged, and the browser REPL typesets it *beside* the text. Turns `pretty` off — a typeset matrix already carries the layout |
| `names on\|off` | Announce each newly-seen free variable (default `off`). A name used as a function it is not — `sqrt(x)` — is reported whatever this is set to |
| `exact on\|off` | Exact rational arithmetic (default `off`); a square root of a fraction stays an exact surd, `√13` or `(3 + 2√2)`, typed with `√` (`√13`, `2√13`, `√(x + 1)`) and saved in ASCII as `13^(1/2)`; a quadratic's irrational roots and a 2-norm come back as exact surds (`norm([[2], [3]])` is `√13`) |
| `exact precision <n>` | Digits an irrational is approximated to (default 30, 1 to 1000) |
| `rationalize(x[, digits])` | Every decimal of `x` as its nearest fraction; the default digits are the fewest that give the small fractions the decimals look like (`1.9999999999999991` → `2`), else the working precision at most 15; a complex value or an interval declines. Offered in a note when an exact input came back as decimals that look exact |
| `simplify <expr>` | Structural simplification, after evaluating every call in the expression (a derivative, a solver, `sin(1)`) as plain evaluation would. Definitions are expanded, numeric bindings are not |
| `expand <expr>` | Distribute products over sums and collect like terms; same preparation as `simplify`. An answer in one variable is written by **descending** degree (`(x + 1)^2` → `x^2 + 2x + 1`); several variables keep the order they first appear in. A sum over a numeric divisor is divided term by term: `(4x3 + 6x4)/2` → `2x3 + 3x4`. A product whose expansion would be deeper than 200 levels (about 200 terms) is left as written, with a note |
| `eval <expr>` | Evaluate substituting current bindings |
| `samples <expr> <v> <lo> <hi> [n]` | Sample function on a grid (default 200 pts, max 100 000) |
| `truth <expr>` | Truth table over the expression's free variables |
| `truth3 <expr>` | Three-valued (Kleene) truth table: false / unknown / true |
| `logic symmetric on\|off` | Spell truth values as -1 / 0 / 1 (default: off) |
| `logic minmax\|product\|lukasiewicz` | Fuzzy t-norm family (default: minmax) |
| `unset <name>` | Remove a binding or definition |
| `:save <file>` | Write session to a replayable script (browser: its own storage); an existing save of that name is replaced, and the answer says so |
| `:load <file>` | Run a session script, restoring its settings, bindings and definitions (browser: its own storage) |
| `quit` / `exit` | Leave the REPL |
| `plot <expr> <v> <lo> <hi> [n]` | **Browser only** — draw `y = f(x)` as a line |
| `points <expr> <v> <lo> <hi> [n]` | **Browser only** — draw the samples as points, axes locked to the same scale |
| `bode <expr> <v> <wMin> <wMax> [n]` | **Browser only** — Bode diagram: dB and unwrapped degrees, log frequency axis (`wMin > 0`) |
| `nyquist <expr> <v> <wMin> <wMax> [n]` | **Browser only** — Nyquist diagram in the complex plane, axes locked |
| `rlocus <G> <v> <kMin> <kMax> [n]` | **Browser only** — root locus: the closed-loop poles of `G` over a gain band (geometric when `kMin > 0`), one continuous line per branch, axes locked |
| `hist <sample> [bins]` | **Browser only** — histogram over the sample's range; the bin count defaults to Sturges' rule. A trailing integer is the count only when what precedes it is a sample (`hist y * 2` is the sample `y * 2`); every cell must be a real number. `bincounts` gives the same counts anywhere |
| `curve <2×1 column> <v> <lo> <hi> [n]` | **Browser only** — trace the column as `v` runs over the range, axes locked |
| `arrows <2×n matrix>` | **Browser only** — an arrow from the origin to each column, axes locked |
| `polygon <2×n matrix>` | **Browser only** — the filled polygon whose vertices are the columns |
| `<draw> ; <draw> ; …` | **Browser only** — several drawing commands as layers of one figure, with a legend (`bode` is drawn alone). `Session.figure(lines*)` renders the same lines as SVG |

---

## Assignment

`=` is always an equation relation, never assignment. Use `:=` for binding.

| Syntax | Meaning |
|---|---|
| `x := 3.14` | Numeric binding (constant right-hand side) |
| `f := sin(x) + x` | Symbolic definition (late-bound: follows redefinitions). When the formula already computes with the current bindings, the echo adds its value now on a second, indented line starting `=`; the definition stays late-bound |
| `h := 2*x = x + 1` | Named equation (pass to `solve`) |
| `g := consolidate(f)` | Freeze the current value of `f` into `g` (not late-bound) |
| `L, U, P := lu(A)` | Tuple binding from a 1×n decomposition result |
| `A := [[1/2, 1], [0, 1]]` | In exact mode an exact matrix is stored as its formula, but a formula naming no variable echoes the matrix it computes (stacked under `pretty on`), so `H := transpose(...)` shows the matrix, not the call |

---

## Expression syntax

| Syntax | Meaning |
|---|---|
| `+` `-` `*` `/` | Arithmetic |
| `^` | Power (right-associative: `2^3^2` = `2^9`) |
| `-x` | Unary minus |
| `3sin(x)` / `3x` | Implicit multiplication |
| `(expr)` | Grouping |
| `√x` | Square root of the factor that follows: `√13`, `2√13` (= `2·√13`), `√(x + 1)`; `√13^2` is `(√13)^2`. The exact surd in exact mode, the `Double` root (as `13^0.5`) otherwise |

### Built-in constants

| Token | Value |
|---|---|
| `pi` | π ≈ 3.14159 |
| `e` | Euler's number ≈ 2.71828 |
| `i` | Imaginary unit (√−1) |
| `inf` | +∞ |
| `-inf` | −∞ |

### Intervals

| Syntax | Result | Notes |
|---|---|---|
| `[lo, hi]` | | A closed interval with **rigorous** bounds (outward rounding); `[a, a]` is the number `a`; `lo > hi` stays as written. In exact mode the endpoints are exact rationals. Displayed outward at the session precision |
| `[1, 2] * [-1, 3]` | `[-2.0, 6.0]` | `+ - * /` and integer/fractional powers |
| `[-2, 1]^2` | `[0.0, 4.0]` | Even power through zero starts at 0 |
| `sin([0, pi])` | `[0.0, 1.0]` | Extrema inside are found |
| `[1, 2] - [1, 2]` | `[-1.0, 1.0]` | **Not** 0: occurrences are independent (correct) |
| `[1, 3] < [2, 4]` | `unknown` | Decided only if it holds for every pair of points |
| `exp ln log sin cos tan asin acos atan sinh cosh tanh abs` | | The covered functions. A divisor containing 0, `tan` across a pole, a domain exceeded, or any other function: stays symbolic |

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
| `sec(x)` / `csc(x)` / `cot(x)` | Secant / cosecant / cotangent (`1/cos`, `1/sin`, `cos/sin`). `simplify` folds the ratio spelling into the reciprocal node, so there is one form of each: `1/cos(x)` → `sec(x)`, `cos(x)/sin(x)` → `cot(x)`, `1/cos(x)^2` → `sec(x)^2` |
| `sech(x)` / `csch(x)` / `coth(x)` | Hyperbolic secant / cosecant / cotangent; `simplify` folds `1/cosh(x)` → `sech(x)` |
| `step(x)` | Heaviside unit step (1 if x ≥ 0, 0 otherwise); `differentiable(step(x), x)` is `x ≠ 0` |
| `maximum(a, b, …)` / `minimum(a, b, …)` | The larger / smaller argument — **element-wise** on matrices, a number spreading over every cell. `maximum` **joins** its arguments. At a tie the **first** argument wins, which is what the derivative uses: `derive(maximum(x, 2*x), x)` is `1` at `x = 0`. `differentiable` excludes the kinks: `differentiable(maximum(0, x), x)` is `x ≠ 0` |
| `max(v)` / `min(v)` | The largest / smallest **entry** of one array, over all entries (shape irrelevant). `max` **reduces** one array |
| `clamp(x, lo, hi)` | `x` limited to `[lo, hi]`, element-wise; bounds may be numbers or columns |
| `softplus(x, k)` | `ln(1 + e^(k·x))/k`, the smooth `maximum(0, x)`; `k > 0` |

The order functions keep an exact value exact (they select, they do not compute) and stay
unevaluated on a complex or truth value and on a name with no value yet.

---

## Numeric sequences

| Syntax | Result | Notes |
|---|---|---|
| `fib(10)` | `55` | Standard indexing: `fib(0)=0`, `fib(1)=1` (OEIS A000045) |
| `fib(100)` | `354224848179261915075` | In exact mode Fibonacci is computed over arbitrary-precision integers: a `Double` stops being exact at `fib(78)`, so this is not an approximation |
| `fib(n, 1, 1)` | | The classic rabbit pair = `fib(n+1)` |
| `fib(5, 1.5, -pi)` | | Any numeric seeds — not just integers |
| `lucas(6)` | `18` | = `fib(n, 2, 1)` |
| `pell(5)` | `29` | `x(n) = 2x(n-1) + x(n-2)` |
| `jacobsthal(5)` | `11` | `x(n) = x(n-1) + 2x(n-2)` |
| `binom(10, 5)` | `252` | Generalised: `binom(-1, 3) = -1` |
| `catalan(5)` | `42` | |
| `harmonic(4)` | `2.0833` | Exact mode gives `25/12` |

### tabulate — a row of terms

| Syntax | Result | Notes |
|---|---|---|
| `tabulate(fib(k), k, 0, 8)` | `[[0, 1, 1, 2, 3, 5, 8, 13, 21]]` | Works for *any* expression, not just the sequences; `k` is a binder |
| `tabulate(binom(4, k), k, 0, 4)` | `[[1, 4, 6, 4, 1]]` | A Pascal row |
| `tabulate(k^2, k, 1, 5)` | `[[1, 4, 9, 16, 25]]` | |
| `at(tabulate(fib(k), k, 0, 10), 1, 11)` | `55` | The result is an ordinary 1×n matrix, so `at` (1-based), tuple assignment and `:save` all apply |

### sum / product — the terms folded

| Syntax | Result | Notes |
|---|---|---|
| `sum(k, k, 1, 100)` | `5050.0` | The same shape as `tabulate`, which yields the *terms* where these yield their total |
| `sum(k^2, k, 1, 4)` | `30.0` | |
| `product(k, k, 1, 5)` | `120.0` | |
| `sum(binom(4, k), k, 0, 4)` | `16.0` | A Pascal row, added up |
| `sum(k*x, k, 1, 3)` | `x + 2x + 3x` | Symbolic terms **expand** |
| `sum(k, k, 1, 0)` | `0.0` | The empty sum; `product` gives 1 |
| `sum(1/k, k, 1, 3)` | `11/6` | Under `exact on`: an exact term stays exact |
| `sum(k, k, 1, n)` | | A free bound stays symbolic; the range is capped like `tabulate`'s, so a mistyped bound declines |

---

## Special functions

| Syntax | Result | Notes |
|---|---|---|
| `fact(5)` | `120.0` | Exact for integers up to `170!`; `171!` stays symbolic |
| `fact(0.5)` | `0.88623` | Analytic continuation, = `Gamma(1.5)` = `sqrt(pi)/2` |
| `dfact(7)` | `105.0` | Double factorial `7!! = 7*5*3*1` |
| `mfact(10, 3)` | `280.0` | Multifactorial, step 3: `10*7*4*1` |
| `Gamma(5)` | `24.0` | `Gamma(n) = (n-1)!`. Capitalised so that lowercase `gamma` remains usable as an ordinary variable name. Poles (0, -1, -2, ...), overflow and complex arguments all stay symbolic rather than returning infinities |
| `Gamma(0.5)` | `1.77245` | = `sqrt(pi)` |
| `lgamma(1e5)` | | `ln\|Gamma\|` — finite where `Gamma` overflows |
| `Beta(1, 4)` | `0.25` | `Gamma(a)Gamma(b)/Gamma(a+b)`; capitalised so that `beta` remains a variable name |
| `Γ(5)` | `24.0` | Greek alias; type it with `ALT-\ g` or `ALT-G`. Parses to the same node; `toString` always emits the ASCII spelling, so `:save` scripts stay portable |
| `β(1, 4)` | `0.25` | Greek alias; type it with `ALT-\ b`. Capital Greek Beta is not accepted: it is a homoglyph of Latin `B` |
| `Si(1)` | `0.94608` | Sine integral (also `Ci`, `Ei`, `li`) |
| `fresnelS(1)` | `0.43826` | Fresnel integrals (also `fresnelC`) |

### Analytic tier

| Syntax | Meaning |
|---|---|
| `erf(x)` / `erfc(x)` | Error function and its complement |
| `digamma(z)` | Logarithmic derivative of `Gamma`; makes `Gamma`/`fact` differentiable |
| `gammaP(a, x)` / `gammaQ(a, x)` | Regularised incomplete gamma (lower / upper) |
| `betaI(x, a, b)` | Regularised incomplete beta |

---

## Probability & statistics

### Distributions — first-class values

| Syntax | Meaning |
|---|---|
| `normal(m, s)` / `uniform(a, b)` / `exponential(l)` | Continuous families |
| `binomial(n, p)` / `poisson(l)` | Discrete families |
| `geometric(p)` | Trials up to and including the first success, support 1, 2, ... |
| `hypergeometric(N, K, n)` | Successes in `n` draws without replacement from `N` items, `K` of them successes |
| `studentt(nu)` / `chisq(k)` / `fdist(d1, d2)` | Student-t, chi-squared and Fisher's F |
| `betadist(a, b)` / `gammadist(k, rate)` | Beta and Gamma; `gammadist` is **shape-rate**, not shape-scale (its mean is `k/rate`) |
| `pdf(d, x)` / `cdf(d, x)` | Density / mass and cumulative distribution |
| `prob(d, lo, hi)` / `quantile(d, p)` | Interval probability and inverse cdf |
| `prob(X < 2)` | Predicate form; `and` intersects, `or` is not supported |
| `expect(d)` / `variance(d)` | A distribution's own moments |
| `expect(e, X)` / `variance(e, X)` | Moments of an expression, by linearity: `expect(2*X + 3, X)` → `2*E[X] + 3` |

### Bayesian inference

| Syntax | Result | Notes |
|---|---|---|
| `bayes(prior, likelihood)` | the normalised posterior | Bayes' theorem over a finite set of hypotheses: two rows (or columns) in, the posterior out in the same shape; exact on exact input |
| `posterior(prior, lik, data)` | an ordinary distribution | Conjugate update; the likelihood is the family with its unknown parameter left **free**, and `pdf`/`cdf`/`quantile`/`expect` apply to the result. Anything outside the three pairs below stays symbolic |
| `posterior(betadist(a, b), binomial(n, p), k)` | `betadist(a + k, b + n - k)` | |
| `posterior(gammadist(k, r), poisson(l), [[x1, x2]])` | `gammadist(k + sum x, r + n)` | |
| `posterior(normal(m0, s0), normal(m, s), [[x1, x2]])` | `normal(mn, sn)` | `s` **known** |

### Seeded sampling and PRBS

| Syntax | Meaning |
|---|---|
| `draw(d, n, seed)` | `n` draws from a distribution, an `n x 1` column, by inverting its cdf; `1 <= n <= 100000`; same seed, same numbers on JVM and browser (continuous: to rounding) |
| `draw(d, n, seed, offset)` | The same stream from its offset-th draw on: `draw(d, 3, s, 5)` = draws 6..8 of `draw(d, 8, s)` |
| `prbs(order, n, seed)` | Maximal-length binary sequence of +1/-1, an `n x 1` column; order 2..20, period `2^order - 1` with one more +1 than -1; the seed picks the phase; `n <= 1000000` |

### Descriptive statistics and inference

| Syntax | Meaning |
|---|---|
| `mean(x)` / `variance(x)` / `stddev(x)` | Sample or distribution; sample variance is the unbiased `n-1` form |
| `pvariance(x)` / `pstddev(x)` | Population forms, dividing by `n` |
| `covariance(x, y)` / `correlation(x, y)` | Two-sample statistics |
| `regress(X, y)` | Least squares by QR; no intercept column added |
| `regress(X, y, lambda)` | Ridge: `min \|X b - y\|^2 + lambda \|b\|^2`, `lambda >= 0`; by QR on `[X; sqrt(lambda) I]`, never `X'X` |
| `wregress(X, y, w)` | Weighted: `min sum w_i (x_i b - y_i)^2`, each `w_i >= 0`; a zero weight drops its row |
| `rls(theta, P, phi, y, lambda)` | One recursive-least-squares update → `[[theta', P']]`; `theta` n x m, `P` n x n, `phi` n x 1, `y` m x 1 (or a number), `lambda` in (0, 1]; use `theta, P := rls(theta, P, ...)` |
| `median(x)` | The middle order statistic; for a distribution, its 0.5 quantile |
| `quantile(x, p)` | Sample quantile, Hyndman-Fan type 7 (R, numpy); a distribution's quantile as before |
| `quantile(x, p, 6)` | Type 6, the `(n+1)p` percentile of most textbooks |
| `sort(x)` | Ascending; a row stays a row, a column a column |
| `bincounts(x, lo, hi, k)` | Counts in `k` equal bins over `[lo, hi]`, a `k x 1` column; half-open bins, the last one closed |
| `ttest(sample, mu)` | Two-sided one-sample t-test p-value |
| `ttest(x, y)` | Two-sample: Welch's test (unequal variances) |
| `ttest(x, y, pooled)` | Two-sample with a pooled variance, `n1 + n2 - 2` dof |
| `ttest(..., less)` / `ttest(..., greater)` | One-sided alternatives (slot words) |
| `proptest(x, n, p0)` | One-proportion z-test p-value |
| `proptest(x1, n1, x2, n2)` | Two-proportion z-test, pooled proportion; `less` / `greater` as for `ttest` |
| `confint(sample, level)` | Confidence interval for the mean, as `[[lo, hi]]` |
| `chisqtest(obs, exp)` | Pearson goodness-of-fit p-value |
| `chisqtest(table)` | Test of independence (or homogeneity) on an r x c table of counts, `(r-1)(c-1)` dof |
| `vartest(x, y)` | F-test of two variances; `less` / `greater` allowed |
| `anova(g1, g2, ...)` | One-way ANOVA p-value over two or more samples |

---
## Optimization

An answer that cannot be proved complete is not given: the call stays as written.

| Syntax | Meaning |
|---|---|
| `stationary(f, x, y)` | Every point where `grad f = 0`, one row per point; `false` when provably none; the REPL classifies each as local minimum / local maximum / saddle; a coupled polynomial gradient goes through an exact lexicographic Groebner basis (capped; past it, declined) |
| `convex(f, x, y)` | `true` (proved from every **principal** minor of the Hessian), `false` (with a witness), else symbolic |
| `lagrange(f, [[g1], [g2]], x, y)` | Stationary points of `f` on `g1 = 0`, `g2 = 0`; a cell may be an equation, one constraint needs no matrix |
| `kkt(f, g, h, x, y)` | The KKT conditions of: minimise `f`, `g <= 0`, `h = 0` (multipliers `mu0..`, `lambda0..`); `0` = absent group |
| `minimize(f, [[x], [y]], x0, m)` | Numeric minimiser as a column; `m` is `gd`, `newton`, `bfgs` or `pbfgs`, always named. A point is returned only if it passes a stationarity test independent of the units, at the precision's tolerance; else the REPL says why |
| `minimize(f, [[x], [y]], x0, lb, ub, m)` | With bounds (`inf` for none): `pbfgs` or `gd` |
| `qp(H, f, A, b, lb, ub)` | `min 1/2 z'Hz + f'z` s.t. `A z <= b`, `lb <= z <= ub`; `H` positive semidefinite (definite: Goldfarb-Idnani; singular: primal active set, `H = 0` is an LP, and an unbounded direction is reported); `0` and `0` = no `A`, `b`; caps 500 variables, 2000 rows; singular `H`: `lp`'s caps (40 variables, 80 rows, 2048-bit coefficients); `-inf` / `inf` = no bound; a number applies to every `z` |
| `qp(H, f, A, b, Aeq, beq, lb, ub)` | The same with `Aeq z = beq`; the REPL names the active constraints, or says why it declined (not convex, infeasible, ...) |
| `lsqfit(f, t, T, Y, [[a], [b]], p0)` | Least squares: fit `f(t; a, b)` to data columns `T`, `Y` |
| `lsqfit(r, [[a], [b]], p0)` | Least squares over a residual column `r` (m x 1); over data: `transpose(tabulate(at(Y,k,1) - ..., k, 1, m))` |
| `lsqfit(..., lb, ub)` / `lsqfit(..., cov)` | Bounds (`inf` for none, a number applies to all); `cov` answers `[[p, C]]`, `C = false` when `m = n` or a bound is active; `cov` is not reserved. Answered only if certified (scale-free cosine test); declined if not identifiable; caps `m*n^2 <= 400000`, 100 iterations |
| `lp(f, A, b, lb, ub)` / `lp(f, A, b, Aeq, beq, lb, ub)` | `min f'x` s.t. `A x <= b`, `lb <= x <= ub`, **exact** (simplex over the rationals, Bland's rule); infeasible and unbounded are proved; `0` = absent group; caps 40 variables, 80 constraints, 2048-bit coefficients |
| `nlp(f, [[x], [y]], x0, g, h, m)` | `min f` s.t. `g <= 0`, `h = 0`, by `m` = `sqp` or `auglag`, always named; `g`, `h` a column, one cell, or `0` = none; a cell may be a relation (`<=`, `>=`, `=`), never strict |
| `nlp(f, [[x], [y]], x0, g, h, lb, ub, m)` | With bounds (`-inf` / `inf` for none). A **local** KKT point, certified (scale-free KKT residual); the REPL names the binding inequalities and their multipliers, or says why it declined |

---
## Calculus

### Differentiation

| Syntax | Result |
|---|---|
| `derive(sin(x), x)` | `cos(x)` |
| `derive(x^3, x)` | `(3.0 * (x ^ 2.0))` |
| `derive(f, x, y)` | Mixed partial ∂²f/∂x∂y |
| `derive(f, x, x)` | Second derivative d²f/dx² |

### Indefinite integration

| Syntax | Result | Notes |
|---|---|---|
| `integral(x^2, x)` | `(1/3) * x^3` | |
| `integral(exp(x), x)` | `exp(x)` | |
| `integral(sin(x), x)` | `-cos(x)` | |
| `integral(1/x, x)` | `ln(x)` | |
| `integral(step(x), x)` | `x*step(x)` | |
| `integral(tan(x), x)` | `-ln(cos(x))` | From the data-driven table |
| `integral(k^x, x)` | `k^x / ln(k)` | Symbolic base free of `x` |
| `integral(1/(a^2 + x^2), x)` | `atan(x/a) / a` | Symbolic parameter `a` |
| `integral(x * exp(x^2), x)` | `exp(x^2)/2` | Non-linear u-substitution (u = x²) |
| `integral(sin(x)^3 * cos(x), x)` | `sin(x)^4/4` | u = sin(x) |
| `integral(tan(x)^3, x)` | `tan²/2 + ln(cos x)` | tan/sec/csc/cot power reduction |
| `integral(1/((x-1)^2*(x-2)), x)` | | Full partial fractions, repeated root |
| `integral(1/((x^2+1)*(x-1)), x)` | | Partial fractions, complex pair |
| `integral(1/(4 - x^2)^0.5, x)` | `asin(x/2)` | Trigonometric substitution |
| `integral((4 - x^2)^1.5, x)` | | Any half-integer power of the radicand |
| `integral(1/(x^2 + 1)^0.5, x)` | `asinh(x)` | Hyperbolic substitution |
| `integral(1/(2 + cos(x)), x)` | | Weierstrass `t = tan(x/2)` |
| `integral(sin(x)/x, x)` | `Si(x)` | Special integral functions |
| `integral(exp(-x^2), x)` | `sqrt(pi)/2 * erf(x)` | |
| `integral(1/ln(x), x)` | `li(x)` | |
| `integral(asin(x), x)` | `x*asin(x) + (1-x^2)^0.5` | From the data-driven table |
| `integral(sin(2*x)*cos(5*x), x)` | | Product-to-sum |
| `integral(1/(a^2 - x^2), x)` | `atanh(x/a)/a` | Symbolic parameter `a` |
| `integral(1/(x^2 + a^2)^0.5, x)` | `asinh(x/a)` | |
| `integral(x^a, x)` | `x^(a+1)/(a+1)` | Symbolic exponent |
| `integral(exp(a*x)*sin(b*x), x)` | | General cyclic pair |

### Vector calculus

A vector field is an n×1 matrix; the coordinate tuple is explicit and **ordered**.

| Syntax | Result | Notes |
|---|---|---|
| `grad(x^2*y, x, y)` | `[[2xy], [x^2]]` | Gradient (n x 1); a vector field given to `grad` stays symbolic |
| `div([[x^2], [y^3]], x, y)` | `2x + 3y^2` | Divergence (scalar) |
| `curl([[-y], [x], [0]], x, y, z)` | `[[0], [0], [2]]` | Curl (3-D only); outside three dimensions it stays symbolic |
| `laplacian(x^3*y, x, y)` | `6xy` | = `div(grad(f))`; a vector field stays symbolic |
| `jacobian([[x*y], [y^2]], x, y)` | `[[y, x], [0, 2y]]` | m x n; stays symbolic in a curvilinear system |
| `hessian(x^2*y^3, x, y)` | | n x n, symmetric; a vector field, or a curvilinear system, stays symbolic |
| `laplacian(1/r, r, t, p, spherical)` | `0` | The Newtonian potential is harmonic |
| `div([[1/r], [0], [0]], r, t, z, cylindrical)` | `0` | The 2-D point source is source-free |
| `grad(f, r, t, z, cylindrical)` | `[[df/dr], [(1/r)*df/dt], [df/dz]]` | |

A component/coordinate count mismatch or a repeated coordinate also stays symbolic rather than
being guessed. An optional trailing keyword selects the coordinate system:

| Keyword | Coordinates, in order | Notes |
|---|---|---|
| `cartesian` | as given | The default |
| `cylindrical` | `(r, θ, z)` | Three-dimensional |
| `spherical` | `(r, θ, φ)`, **θ the polar angle** | The physics convention: pass the polar angle second |
| `sphericalmaths` | `(r, θ, φ)`, **θ azimuthal, φ polar** | The mathematics convention: pass the polar angle third |

The curvilinear systems are three-dimensional, and the coordinates are identified by
**position, not by name** — call them whatever you like, but pass them in that order; the two
spherical keywords therefore differ in argument *order*, not in naming.

### Definite integration (Simpson's rule)

The limits are ordinary **expressions**, so arithmetic, constants and other variables are all
legal there.

| Syntax | Result | Notes |
|---|---|---|
| `integral(sin(x), x, 0, pi)` | ≈ `2.0` | |
| `integral(sin(x), x, 0, 2*pi)` | ≈ `0.0` | Over a full period |
| `integral(sin(x), x, -100*pi, 100*pi)` | | An expression bound is not a special case |
| `integral(integral(1, y, 0, 2*x), x, 0, 1)` | `1.0` | Iterated, inner limit in the outer variable |

### Limits

| Syntax | Result | Notes |
|---|---|---|
| `limit(sin(x)/x, x, 0)` | `1.0` | |
| `limit(1/x, x, 0, +)` | `inf` | From the right |
| `limit(1/x, x, 0, -)` | `-inf` | From the left |
| `limit(atan(x), x, inf)` | `1.5708` | π/2 |
| `limit((1 + 1/x)^x, x, inf)` | `2.71828` | e |

### Sampling

| Command | Notes |
|---|---|
| `samples sin(x) x -pi pi` | |
| `samples f x 0 10 500` | 500 points for defined function `f` |

---

## Series expansions

| Syntax | Result / notes |
|---|---|
| `maclaurin(exp(x), x, 4)` | `1 + x + x^2/2 + x^3/6 + x^4/24`. `maclaurin(e, v, n)` is sugar for `taylor(e, v, 0, n)` |
| `maclaurin(sin(x), x, 7)` | Odd powers only |
| `maclaurin(cos(x), x, 6)` | Even powers only |
| `maclaurin(1/(1-x), x, 5)` | The geometric series |
| `taylor(exp(x), x, 1, 4)` | Expanded about `x = 1`. The expansion variable stays free in the result, so binding it evaluates the polynomial. Order is capped at 20 |
| `taylor(x^2, x, a, 2)` | Symbolic centre: a polynomial in `(x - a)` |
| `taylor(tan(x), x, 0, 20)` | Quick: coefficients come from power-series arithmetic, and an exact input keeps exact coefficients |
| `taylor(erf(sin(x)), x, 0, 20)` | Declines with a note naming the order rather than running for minutes: a function with no recurrence (`erf`, the special functions) is differentiated instead, each derivative held to 50 000 nodes. An expression whose derivative stays symbolic (`digamma`) returns unevaluated rather than a bogus series. `taylor`, `laurent` and `pade` all say why they declined |
| `fourierSeries(x, x, 2*pi, 4)` | Sawtooth: `2sin(x) - sin(2x) + (2/3)sin(3x) - ...`. Expands over one period centred on 0, coefficients computed numerically by Simpson — so the period must be a concrete positive number. A different operation from `fourier(e, t, w)`, the Fourier *transform* |
| `fourierSeries(x^2, x, 2*pi, 4)` | Even function: cosine terms only, `a0/2 = pi^2/3`. Coefficients are not chopped: a term that vanishes analytically returns at the integrator noise floor |
| `fourierSeries(sin(pi*x), x, 2, 3)` | Period 2, so omega = pi |
| `pade(exp(x), x, 1, 1)` | `(2 + x)/(2 - x)`, the rational `[m/n]` approximant |
| `pade(exp(x), x, 2, 2)` | Beats `maclaurin(exp(x), x, 4)` away from 0 |
| `pade(1/(1+x), x, 0, 1)` | A rational function reproduces itself exactly |
| `laurent(1/(x*(x-1)), x, 0, 3)` | Laurent series about a pole |
| `laurent(sin(x)/x^2, x, 0, 1, 3)` | The principal-part length is optional (detected via `singularities`) |

---
## Equations & solving

### Equation relations

| Syntax | Meaning |
|---|---|
| `2*x + 1 = 5` | Symbolic equation (evaluates to `_Bool` when `x` is bound) |
| `x^2 + 1 == x^2 + 1` | Equality check (not solvable via `solve`). Two numbers are equal within `0.5·10^-precision`; two **exact** values when they agree to the working precision, `\|a − b\| <= 10^-wp·max(\|a\|, \|b\|, 1)`, so `(2^(1/2))^2 == 2` is `true` while `1/3 == 0.33333` stays `false`. When the tolerance, not exact equality, decided, the REPL notes: *the two sides agree to the working precision (N digits), not exactly* |

### solve — single equation

| Syntax | Result | Notes |
|---|---|---|
| `solve(2*x + 1 = 5, x)` | `x = 2.0` | Linear |
| `solve(x^2 = 4, x)` | `[[x = -2.0, x = 2.0]]` | Quadratic; in exact mode irrational roots are exact surds (`x^2 = 13` gives `±√13`) |
| `solve(sin(x) = 0, x)` | | Numeric bisection fallback |

### solve — inequalities

The answer is written in the language itself — a comparison, or a union/intersection of
comparisons via `or`/`and`.

| Syntax | Result | Notes |
|---|---|---|
| `solve(x^2 - 4 > 0, x)` | `((x < -2.0) or (x > 2.0))` | |
| `solve(2*x + 1 <= 5, x)` | `(x <= 2.0)` | |
| `solve(2*x > 2 and x < 5, x)` | `((x > 1.0) and (x < 5.0))` | |
| `solve(a*x > b, x)` | stays symbolic | The sign of `a` is unknown, and dividing by a negative coefficient would flip the relation |

### Comparisons

| Syntax | Meaning |
|---|---|
| `x < 2` | Also `>` `<=` `>=` `!=`; reduce to true/false when bound |
| `x > 0 and x < 10` | Compose with `and`/`or`/`not`/`implies` |

### solve — matrix equation

| Syntax | Result | Notes |
|---|---|---|
| `solve(A * X = B, X)` | `X = inv(A) * B` | |
| `solve(X * A = B, X)` | `X = B * inv(A)` | |
| `solve(A * X * D = B, X)` | `X = inv(A) * B * inv(D)` | |
| `solve(A * X + C = B, X)` | `X = inv(A) * (B - C)` | Affine |
| `solve(A * X + X * B = C, X)` | | Sylvester equation (Kronecker vectorization; Lyapunov `A * X + X * transpose(A) = C` alike). Numeric: up to 400 vectorized unknowns. Exact matrices: solved over the rationals with an exact answer, up to 36 vectorized unknowns; past it the equation stays unevaluated |

### solveSystem — linear systems

| Syntax | Result | Notes |
|---|---|---|
| `solveSystem([[2*x + y = 3, x - y = 0]], x, y)` | `[[x = 1.0, y = 1.0]]` | |
| `solveSystem([[2*x + y = 3], [x - y = 0]], x, y)` | | The same, as a column |
| `solveSystem([[u = 1], [v = 2], [u + v = 3]], u, v)` | | More equations than unknowns: solved when consistent. Numeric consistency is relative to each equation's own terms, at the display precision; nearly dependent numeric columns are declined first |
| `solveSystem([[u = 1], [v = 2], [u + v = 4]], u, v)` | | Inconsistent: unevaluated, the note names equation 3 |

Fewer equations than unknowns, a non-unique solution, a non-linear system and a symbolic
over-determined system stay unevaluated, with the reason as a note.

---

## Domain analysis

Where an expression is defined, differentiable, or singular — answered in the language, as
comparisons and connectives.  The analysis describes what the *library* computes, not the
full analytic extension.

| Syntax | Result | Notes |
|---|---|---|
| `domain(ln(x - 2), x)` | `(x > 2.0)` | |
| `domain(asin(x), x)` | `((x >= -1.0) and (x <= 1.0))` | |
| `domain(1/x, x)` | `(x != 0.0)` | |
| `differentiable(ln(x), x)` | `(x > 0.0)` | Where the derivative exists |
| `singularities(1/(x-1)^2, x)` | `[[1.0], [2.0]]` | Location over pole order |
| `singularities(x^2, x)` | `false` | Provably none |
| `domain(tan(x), x)` | stays symbolic | The exclusion set is infinite |

---

## Base conversion

The base lives **on the value**: `A := 0xFF` still prints `0xFF` after a `:save`/`:load`
round-trip, while remaining usable as `255` everywhere.

| Syntax | Result | Notes |
|---|---|---|
| `0b1011` | `1011` | Binary literal (= 11) |
| `0o17` | `17` | Octal (= 15) |
| `0xff` | `FF` | Hexadecimal (= 255) |
| `0t1TT` | `1TT` | Balanced ternary, digits 1, 0, T = -1 (= 5) |
| `tobase(255, 16)` | `FF` | Any radix 2..36 |
| `balanced(5)` | `1TT` | Balanced ternary conversion |
| `0xFF + 1` | `256` | Arithmetic is decimal: the tag is display-only |

---

## Logic

### Connectives

| Syntax | Result / meaning |
|---|---|
| `true and false` | `false` (`true`/`false` are literals) |
| `not a` | Negation |
| `a implies b` | Material implication (right-associative) |
| `a xor b` | Exclusive or |
| `a or b and c` | = `a or (b and c)`. Precedence, tightest to loosest: `not` > `and` > `xor` > `or` > `implies` |
| `x = 1 and y = 2` | Connectives bind looser than `=` |
| `false and (1/0 = 0)` | `false` (short-circuit: right side never evaluated) |
| `cnf(a xor b)` | Conjunctive normal form: `((a or b) and ((not b) or (not a)))`; stays as written for a graded truth value, or past the clause budget |
| `dnf((a or b) and not a)` | Disjunctive normal form: `(b and (not a))`; the same limits as `cnf` |

### Three-valued (Kleene) logic

| Syntax | Result | Notes |
|---|---|---|
| `unknown` | | The third truth value (a bindable value like `true`/`false`) |
| `not unknown` | `unknown` | 0.5 is the negation fixpoint |
| `false and unknown` | `false` | 0 annihilates min |
| `true or unknown` | `true` | 1 annihilates max |
| `unknown and unknown` | `unknown` | |
| `unknown implies unknown` | `unknown` | |

### Symmetric ternary (an encoding, not a semantics)

| Syntax | Result | Notes |
|---|---|---|
| `logic symmetric on` | | Spell truth values as -1 / 0 / 1, related by `t = (s + 1) / 2`; the rule table is identical either way. `:save` persists the toggle but always writes the word spelling |
| `logic symmetric off` | | `false` / `unknown` / `true` (default) |
| `logic symmetric` | | Show the current setting |
| `-1 and 0` | `-1` | `false and unknown`, with the toggle on |
| `1 or 0` | `1` | |
| `0 and 0` | `0` | |
| `not 0` | `0` | |
| `2 * 3 + 1` | `7.0` | Arithmetic is never reinterpreted |
| `1 and 0` | stays symbolic | While the toggle is **off** |

### Fuzzy logic ([0, 1] degrees)

| Syntax | Result | Notes |
|---|---|---|
| `truth(0.3)` | | A graded degree (also how one prints, so it round-trips) |
| `truth(0.3) and truth(0.7)` | `truth(0.3)` | min-max |
| `not truth(0.3)` | `truth(0.7)` | |
| `very(0.5)` | `truth(0.25)` | Concentration: d² |
| `somewhat(0.25)` | `unknown` | Dilation: √d = 0.5 |
| `trimf(x, a, b, c)` | | Triangular membership curve |
| `trapmf(x, a, b, c, d)` | | Trapezoidal |
| `gaussmf(x, mean, sigma)` | | Gaussian |
| `sigmf(x, a, c)` | | Sigmoid |
| `defuzz(trimf(x,0,5,10), x, 0, 10)` | `5.0` | Crisp value by centre of gravity |
| `defuzz(e, x, lo, hi, mom)` | | By the mean of the maxima; `bisector` splits the area, `centroid` is the default (slot words, not reserved) |

### Custom membership curves

| Syntax | Result | Notes |
|---|---|---|
| `truth(1 / (1 + (x - 5)^2))` | | A Cauchy bell — a curve of your own: the four built-in shapes are a convenience, not a limit, and `truth(<expr>)` turns **any** scalar expression of one variable into a membership curve |
| `bell := truth(1 / (1 + (x-5)^2))` | | Name it, then use the name |
| `defuzz(bell, x, 0, 10)` | `5.0` | Custom curves defuzzify like built-ins |
| `very(bell)` | | And compose with hedges and connectives |
| `truth(2)` | stays symbolic | A degree must lie in [0, 1] |
| `defuzz(1/(1+(x-5)^2), x, 0, 10)` | | A bare scalar curve works too (no wrapper needed) |
| `defuzz(trimf(x,0,3,6) or trimf(x,4,7,10), x, 0, 10)` | | Aggregate curves with the connectives, then defuzzify |

### t-norm families

| Command | Meaning |
|---|---|
| `logic minmax` | `and` = min, `or` = max (default; the only lattice). Idempotence and absorption hold for graded degrees under `minmax` only |
| `logic product` | `and` = `a*b`, `or` = `a+b-a*b` |
| `logic lukasiewicz` | `and` = `max(0,a+b-1)`, `or` = `min(1,a+b)` |
| `logic` | Show both logic settings |

All three agree with classical logic on true/false, so the boolean and ternary tiers are
unaffected.

### Simplification & truth tables

| Command | Result | Notes |
|---|---|---|
| `simplify a and true` | `a` | |
| `simplify not not a` | `a` | |
| `simplify a or (a and b)` | `a` | Absorption |
| `simplify unknown and not unknown` | `unknown` | Complement is gated on crisp operands |
| `truth a and b` | | Two-valued table over the free variables (max 16) |
| `truth3 a and not a` | | Three-valued table: false / unknown / true (max 10) |

## Complex numbers

`i` is the imaginary unit; arithmetic with `i` produces complex results automatically.

| Syntax | Result | Notes |
|---|---|---|
| `(2 + 3i) * (1 - i)` | `(5.0 + 1.0i)` | |
| `exp(i * pi)` | `-1.0` | Euler's formula |
| `(-1)^0.5` | `(0.0 + 1.0i)` | Principal value |
| `ln(-1)` | `(0.0 + 3.14159i)` | |

---

## Matrix domain

### Literals

| Syntax | Meaning |
|---|---|
| `[[1, 2], [3, 4]]` | 2×2 matrix |
| `[[1, 2, 3]]` | 1×3 row vector |

### Constructors

| Syntax | Meaning |
|---|---|
| `eye(n)` | n×n identity matrix |
| `zeros(r, c)` | r×c zero matrix |
| `zeros(n)` | n×n zero matrix |
| `ones(r, c)` / `ones(n)` | All-ones matrix |

### Block matrices — a number counts as a 1×1 block in hcat/vcat/blkdiag/kron

| Syntax | Result / meaning |
|---|---|
| `hcat(A, B, …)` | Side by side (equal heights). A free name is never guessed to be a scalar: `hcat(x, A)` waits until `x` is bound |
| `vcat(A, B, …)` | Stacked (equal widths) |
| `blkdiag(A, B, …)` | On the diagonal, zeros elsewhere |
| `repmat(A, m, n)` | `A` tiled `m` down, `n` across |
| `kron(A, B)` | Kronecker product |
| `submatrix(A, r0, r1, c0, c1)` | Rows `r0..r1`, cols `c0..c1` — **1-based, inclusive** (like `at`); `sub` is **not** reserved |
| `kron(eye(2), [[1, 2]])` | `[[1, 2, 0, 0], [0, 0, 1, 2]]` |
| `submatrix(A, i, i, j, j)` | The 1×1 matrix holding `at(A, i, j)` |

A result above 1 000 000 cells is refused before it is built.

### Operations

| Syntax | Meaning |
|---|---|
| `A + B` | Element-wise sum |
| `A - B` | Element-wise difference |
| `A * B` | Matrix product (or scalar multiple if one is a number) |
| `k * A` | Scalar multiple |
| `transpose(A)` | Transpose |
| `det(A)` | Determinant (scalar) |
| `inv(A)` | Inverse |
| `A ^ n` | Integer matrix power (binary exponentiation) |
| `pow(A, n)` | Same as `A^n` |
| `trace(A)` | Diagonal sum of a square matrix (scalar) |
| `size(A)` | The row `[[rows, cols]]`; `r, c := size(A)` names both |
| `size(A, 1)` / `size(A, 2)` | Number of rows / columns (scalar); a number is 1x1 |

### Vector products and norms

The shape decides: a row or a column is a vector, anything else a matrix.

| Syntax | Meaning |
|---|---|
| `dot(u, v)` | Σ uᵢvᵢ, a number; rows, columns or one of each; no conjugate |
| `cross(u, v)` | Right-handed product of two 3-vectors, in `u`'s shape |
| `norm(x)` | 2-norm: a vector's length, a matrix's largest singular value. The math editor's `∥x∥` reads as `norm(x)` |
| `norm(x, 1)` / `norm(x, inf)` | Vector: sum / largest modulus; matrix: max column / row sum |
| `norm(x, fro)` | Frobenius: the 2-norm of every cell together |
| `norm(v, p)` | Vector p-norm, any `p >= 1` |

### Decompositions — result is a 1×n row, use `at(result, 1, k)` to index

| Syntax | Result | Notes |
|---|---|---|
| `lu(A)` | `[[L, U, P]]` | `P·A = L·U`, partial pivoting; exact on exact `A`, singular included |
| `qr(A)` | `[[Q, R]]` | `A = Q·R`, modified Gram-Schmidt; dependent columns: unevaluated, with a note |
| `eigen(A)` | `[[λ₁, …, λₙ]]` | Eigenvalues only. `eigen`, `eig` and `jordan` list eigenvalues by real part ascending, then imaginary part — the order `tfpoles` answers in, so a conjugate pair comes `−im` first |
| `eig(A)` | `[[V, D]]` | `A·V = V·D` (eigenvectors + diagonal); defective: unevaluated; `V`'s columns are permuted with `D` |
| `jordan(A)` | `[[P, J]]` | `A = P·J·P⁻¹`; exact with blocks when exact, eigenvalues rational |

### Matrix exponential — a single matrix, not a row

| Syntax | Result | Notes |
|---|---|---|
| `expm(A)` | e^A | Scaling + squaring, degree-13 Padé. **Not** `A^n`: that is repeated multiplication, this is the exponential series. Works for **defective** matrices (a repeated eigenvalue with too few eigenvectors), where the eigen-decomposition route has no basis to use |
| `expm(zeros(3, 3))` | `eye(3)` | |
| `expm([[0, 1], [0, 0]])` | `[[1, 1], [0, 1]]` | Nilpotent: series terminates |
| `expm([[0, -t], [t, 0]])` | rotation by `t` | |

### Cholesky factor — a single matrix, not a row

| Syntax | Result | Notes |
|---|---|---|
| `chol(A)` | `L`, lower triangular, `A = L·Lᵀ` | Stays unevaluated unless `A` is square, symmetric and positive definite — the refusal *is* the convexity test. Tolerances are relative to the largest entry; exact input stays exact |
| `chol([[4, 2], [2, 3]])` | `[[2, 0], [1, 1.41421]]` | |
| `chol([[1, 2], [2, 1]])` | stays `chol(...)` | Indefinite |

### Singular values

| Syntax | Result | Notes |
|---|---|---|
| `svd(A)` | `[[U, S, V]]` | `A = U·S·Vᵀ`, economy (U m×k, S k×k, V n×k). Exact input demotes to `Double`; past `max(m, n)·min(m, n)² = 10⁷` the call stays unevaluated |
| `svd(A, full)` | U m×m, S m×n, V n×n | `full` is a slot word, not reserved |
| `U, S, V := svd(A)` | | Tuple binding |
| `rank([[1, 2], [2, 4]])` | `1` | `rank`, `pinv`, `nullspace` and `orth` count a singular value as zero at or below `0.5·10⁻ᵖ·σ_max` (`p` = precision), so rescaling `A` never changes the answer |
| `rank(A, 0.01)` | | Tolerance relative to the largest singular value, in [0, 1) |
| `pinv(A)` | `V·S⁺·Uᵀ` | = `inv(A)` for square non-singular `A` |
| `cond([[1, 0], [0, 0.001]])` | `1000` | |
| `cond([[1, 2], [2, 4]])` | `inf` | `cond` is `inf` only when `σ_min` is lost in rounding |
| `nullspace(A)` / `nullspace(A, tol)` | n×(n − rank) | Orthonormal basis of {x : A·x = 0}; `false` if trivial |
| `orth(A)` / `orth(A, tol)` | m×rank | Orthonormal basis of the range; `false` for the zero matrix |

### Indexing decomposition results

| Syntax | Meaning |
|---|---|
| `L, U, P := lu(A)` | Tuple binding |
| `at(lu(A), 1, 1)` | First element (`L`) |
| `at(eigen(A), 1, 2)` | Second eigenvalue |

### Scalar functions over matrices (element-wise)

| Syntax | Meaning |
|---|---|
| `sin([[pi/2, 0], [0, pi]])` | Element-wise sin |
| `exp([[1, 0], [0, 1]])` | Element-wise exp |

---

## Integral transforms

### Laplace transform  `L{e(t)}`

| Syntax | Result | Notes |
|---|---|---|
| `laplace(1, t, s)` | `1/s` | |
| `laplace(t^2, t, s)` | `2/s^3` | |
| `laplace(sin(3*t), t, s)` | `3/(s^2+9)` | |
| `laplace(exp(2*t)*cos(t), t, s)` | `(s-2)/((s-2)^2+1)` | First-shift |
| `laplace(t^3*exp(-t), t, s)` | | Derivative-of-transform rule |

### Fourier transform  `F{e(t)} = ∫ e(t)·e^(−iωt) dt` over the whole line

**Two-sided**: `L{e(t)}(iω) + L{e(−t)}(−iω)`. `fourierCausal` is the transform of `e(t)·u(t)`.

| Syntax | Result | Notes |
|---|---|---|
| `fourier(exp(-abs(t)), t, w)` | `2/(1 + w^2)` | |
| `fourier(step(t+1) - step(t-1), t, w)` | | The centred pulse, `2*sin(w)/w`: a pole that cancels in the whole transform is fine |
| `fourier(step(t)*exp(-2*t), t, w)` | `1/(2 + i*w)` | |
| `fourierCausal(exp(-2*t), t, w)` | `1/(2 + i*w)` | |
| `fourier(1, t, w)` | unevaluated, with a note | A pole on the axis. Answered only when every pole of each Laplace part lies strictly left of the imaginary axis: `1`, `cos(t)` and `step(t)` need Dirac deltas, `exp(-t)` diverges as `t → −∞`, and a pole whose side depends on a free parameter cannot be placed; all stay unevaluated with a note saying why |

### Inverse Laplace transform  `L⁻¹{f(s)}`

| Syntax | Result |
|---|---|
| `invlaplace(1/s, s, t)` | `1` |
| `invlaplace(1/s^2, s, t)` | `t` |
| `invlaplace(1/(s-3), s, t)` | `exp(3*t)` |
| `invlaplace(2/(s^2+4), s, t)` | `sin(2*t)` |
| `invlaplace(3/((s-2)^2+9), s, t)` | `exp(2*t)*sin(3*t)` |

### z-transform  `X(z) = Σ(n≥0) x[n]·z⁻ⁿ`

**One-sided**, like `laplace`. Argument order is `(sequence, index, z)`.

| Syntax | Result | Notes |
|---|---|---|
| `ztrans(1, n, z)` | `z/(z-1)` | Constant **sequence**, not `1/z`: `Z{c} = c·z/(z−1)`, because the Laplace `L{c} = c/s` does not carry over — a constant sequence is `c` at every index |
| `ztrans(2^n, n, z)` | `z/(z-2)` | |
| `ztrans(n, n, z)` | `z/(z-1)^2` | |
| `ztrans(n^2, n, z)` | `z*(z+1)/(z-1)^3` | |
| `ztrans(n*2^n, n, z)` | `2*z/(z-2)^2` | |
| `ztrans(sin(0.5*n), n, z)` | `z*sin(0.5)/(z^2-2*z*cos(0.5)+1)` | |
| `invztrans(z/(z-2), z, n)` | `2^n` | Complex poles, the Kronecker delta and the bilateral transform are not supported and stay symbolic |
| `invztrans(z/((z-1)*(z-2)), z, n)` | `2^n - 1` | |
| `invztrans(z/(z-2)^2, z, n)` | `n*2^(n-1)` | |

### Discrete Fourier transform and convolution

| Syntax | Meaning |
|---|---|
| `dft(x)` | `X[k] = sum x[n] e^(-2 pi i k n / N)`: a row or column, real or complex; the shape is kept; `N <= 8192`. The DTFT needs no function of its own: it is `ztrans` with `z := exp(i*w)` |
| `idft(X)` | The inverse, with the `1/N` |
| `conv(x, h)` | Linear convolution, length `n + m - 1`; exact on exact input; `n*m <= 2.5e7` products, or `4e4` when exact |

---

## Control systems

A transfer function is **an ordinary expression** — no carrier type — so the frequency
variable is always an explicit argument. Grammar forms first, then the library API.

| Syntax | Meaning |
|---|---|
| `series(g, h, s)` | G·H, normalised to one rational in lowest terms |
| `parallel(g, h, s)` | G + H |
| `feedback(g, h, s)` | G/(1 + G·H), **negative** feedback; negate `h` for positive |
| `step(g, s, t)` | Step response = `invlaplace(G/s, s, t)`. Arity-overloaded: `step(x)` is the Heaviside unit step, `step(G, s, t)` the response |
| `impulse(g, s, t)` | Impulse response = `invlaplace(G, s, t)` |
| `tfpoles(g, s)` / `tfzeros(g, s)` | Roots as a row, sorted; `false` for provably none. `tfzeros`, not `zeros`: `zeros(r, c)` is the zero matrix. The analysis words read `G` with the session's bound parameters but never with a binding of the frequency variable itself; where they cannot answer, the call stays as written with a note |
| `isStable(g, s)` | Every pole strictly in the **left half-plane**; marginal stability is **not** stability: poles on the imaginary axis give `false` |
| `isStableDiscrete(g, z)` | Every pole strictly **inside the unit circle** |
| `routhTable(g, s)` | The Routh array of the denominator as a matrix, rows padded with zeros; `routh` itself stays a legal variable name |
| `stateSpace(A, B, C, D)` | `[[A, B, C, D]]`, the model row |
| `controllable(A, B)` | `true` when the input reaches every state (QR rank test); also takes the model row: `controllable(sys)` |
| `observable(A, C)` | Observability, by the duality; also takes the model row |
| `ctrb(A, B)` / `obsv(A, C)` | The controllability and observability matrices |
| `acker(A, B, poles)` | Single-input pole placement by Ackermann: `K` with the eigenvalues of `A - B*K` at the poles (a row, conjugate pairs together); `u = -K*x`; certified, else declined |
| `dlsim(sys, u, x0)` | Outputs p x N of `x[k+1] = Ax + Bu`, `y = Cx + Du`; `u` is m x N; `x0 = 0` is rest, `D = 0` no feedthrough |
| `dlsim(sys, u, x0, states)` | `[[Y, X]]`, `X = x[0] .. x[N]`: the **last** column continues a run |
| `c2d(g, s, z, Ts, method)` | Discretise; the method is always written: `zoh`, `tustin`, `foh`, `impulse` (aliases!), `matched`, `forward` (can make a stable plant unstable), `backward`; `zoh`, `foh` and `impulse` decline a complex pole pair (no z-table entry) |
| `c2d(g, s, z, Ts, tustin, w)` | Tustin prewarped to match exactly at `w` rad/s |
| `dare(A, B, Q, R)` | The **stabilising** `P` of `AᵀPA − P − AᵀPB(R+BᵀPB)⁻¹BᵀPA + Q = 0`; `R` sym. pos. definite, `Q` sym. pos. semidefinite; n <= 200. Declines, saying why, when no stabilising solution exists: `(A, B)` not stabilisable, or an unstable mode `Q` does not penalise |
| `dlqr(A, B, Q, R)` | `[[K, P]]`, the law `u = −K·x`; `A − B·K` proved stable; declines as `dare` does |
| `dlyap(A, Q)` | `X` with `A·X·Aᵀ − X + Q = 0`, `A` stable |
| `kalmanGain(A, C, Qw, Rv)` | `[[M, P]]`: steady update gain, a priori covariance (dual DARE). Noise model for the filter: `x[k+1] = Ax + Bu + w`, `y = Cx + Du + v`, `w ~ N(0, Qw)`, `v ~ N(0, Rv)` |
| `kalmanStep(xhat, P, u, y, sys, Qw, Rv)` | `[[xhat', P']]`: update, then predict; the output is the next input |

| Example | Result | Notes |
|---|---|---|
| `feedback(1/(s*(s+2)), 1, s)` | `1/(s^2 + 2*s + 1)` | Common factor cancelled |
| `step(1/(s+1), s, t)` | `1 - exp(-t)` | |
| `impulse(2/(s+3), s, t)` | `2*exp(-3*t)` | |
| `tfpoles(1/(s^2 + 3*s + 2), s)` | `[[-2, -1]]` | |
| `isStable(1/(s^2 + 1), s)` | `false` | Marginal is not stable |

Library API (`import it.grypho.scala.leonardo.control.*`; at the prompt `poles`/`zeros` are
`tfpoles`/`tfzeros`):

| Signature | Returns | Notes |
|---|---|---|
| `poles(g, s)` | `Option[Vector[_Value]]` | Denominator roots, in lowest terms |
| `zeros(g, s)` | `Option[Vector[_Value]]` | Numerator roots (`_Complex` when oscillatory) |
| `dcgain(g, s)` | `Option[Double]` | G(0) |
| `isStable(g, s)` | `Option[Boolean]` | Every pole strictly in the **left half-plane** |
| `isStableDiscrete(g, z)` | | Every pole strictly **inside the unit circle** |
| `routhTable(g, s)` | `Option[Vector[Vector[Double]]]` | Jagged rows; at the prompt the same name answers a padded matrix |
| `rootLocus(g, s, gains)` | `Option[branches]` | Closed-loop poles per gain, matched into branches |
| `bode(g, s, w)` | `Option[(magnitude, phase-radians)]` | One frequency |
| `nyquist(g, s, w)` | `Option[(real, imaginary)]` | One frequency |
| `frequencyResponse(g, s, wMin, wMax, n, env)` | `Vector[(omega, dB, degrees)]` | **Geometric** grid, phase **unwrapped** past -180; `wMin > 0`: `log(0)` is not a number |
| `nyquistSweep(g, s, wMin, wMax, n, env)` | `Vector[(real, imaginary)]` | The same grid |
| `stateSpace(a, b, c, d)` | 1x4 `_Matrix` of matrices | The `lu`/`qr`/`eig` shape |
| `linearize(F, G, x, u, x0, u0)` | `[[A, B, C, D]]` | At `(x0, u0)`; `x`, `u` ordered: `[[x1], [x2]]` or a name. Also at the prompt; a point outside the domain is declined |
| `linearize(..., affine)` | `[[A, B, C, D, F0, G0]]` | Offsets for a non-equilibrium point |
| `mpcMatrices(A, B, C, Np, Nc)` | `[[Phi, Gamma]]` | `Y = Phi*x + Gamma*U`; last move **held** past `Nc`. Also at the prompt; a number is a 1x1 matrix |
| `arx(u, y, na, nb, nk)` | `[[a, b]]` | `y[k] = sum a_i y[k-i] + sum b_l u[k-nk-l+1]`. **Predictor** sign: MATLAB's `a` is `-a`. `u` m x N (or N x m), `y` 1 x N; `b` is nb x m, a column per input; `a` is `0` when `na = 0`. Declines a short series and data that does not excite the model. `y` p x N (several outputs): p x p blocks **stacked**, `a` (na·p) x p, `b` (nb·p) x m; `a` is `zeros(p, p)` when `na = 0` |
| `arxMimo(u, y, na, nb, nk, env)` | `ArxMimoModel(a, b)` | The blocks `A_i` (p x p), `B_l` (p x m) |
| `arxRegressors(u, y, na, nb, nk)` | `[[X, Y]]` | The regression `arx` solves; `Y` p columns |
| `arx2tf(a, b, nk, z)` | `B(z)/A(z)` | One per input (a 1 x m row for several); `A(z)^-1 B(z)`, p x m, for stacked blocks (p <= 6) |
| `arx2ss(a, b, nk)` | `[[A, B, C, D]]` | Observer canonical form; `nk` is always explicit; block observer form for stacked blocks: **not** minimal. `arx`, `arxRegressors`, `arx2tf` and `arx2ss` are all also at the prompt |
| `controllable(a, b)` / `observable(a, c)` | `Option[Boolean]` | |
| `c2dExact(a, b, ts)` | `(A_d, B_d)` | Via `expm` of the block matrix; singular `A` is fine |
| `c2d(g, s, z, ts, method)` | `Option[_Expression]` | The method is never a hidden default |
| `c2dPrewarped(g, s, z, ts, w)` | `Option[_Expression]` | Tustin matched exactly at `w` |
| `d2c(g, z, s, ts, method)` | `Option[_Expression]` | Tustin and the Euler methods only |
| `d2cPrewarped(g, z, s, ts, w)` | `Option[_Expression]` | |

A non-rational `G` (a dead-time `exp(-2*s)`), an improper `G` in a time response, and an
undeterminable coefficient sign are all **declined**, never approximated.

---

## Ordinary differential equations

First-order IVP  `y' = f(t, y)`,  `y(t₀) = y₀`,  returns `y(target)`.

| Syntax | Problem | Result / notes |
|---|---|---|
| `ode(k*y, y, t, 0, 1, 1)` | `y' = k·y`, `y(0)=1` | `e^k` (symbolic in `k`) |
| `ode(-y, y, t, 0, 1, 1)` | `y' = -y`, `y(0)=1` | `0.36788` (= 1/e) |
| `ode(-y+t, y, t, 0, 1, 1)` | `y' = -y+t`, `y(0)=1` | Integrating factor (closed form): the closed-form tier covers linear `y' = a(t)·y + b(t)` |
| `ode(y*y, y, t, 0, 1, 0.5)` | nonlinear | RK4 (4th-order Runge-Kutta), the numeric fallback for all other shapes |

Linear **systems** `y' = A·y + b` with a column `y0` (`A` constant and square, `b` a column):

| Syntax | Result | Notes |
|---|---|---|
| `ode([[0, 1], [-1, 0]] * y, y, t, 0, [[1], [0]], 1)` | `[[cos 1], [-sin 1]]` | `e^(A·τ)·y₀` by the augmented matrix exponential (the `c2dExact` kernel), so a defective or singular `A` is fine. Numeric only; a non-conforming shape stays symbolic |
| `A := [[0, 1], [0, 0]]` | | A bound `A` works the same |
| `ode(A*y + [[0], [1]], y, t, 0, [[0], [0]], 2)` | `[[2.0], [2.0]]` | Singular `A` |

**Time-varying and nonlinear systems** are integrated numerically. The state is one name bound
to the whole column, or a **column of names**, each bound to its component:

| Syntax | Result | Notes |
|---|---|---|
| `ode([[v], [-x]], [[x], [v]], t, 0, [[1], [0]], 1)` | `[[0.5403], [-0.84147]]` | RK4, **certified by step doubling**: `n` and `2n` steps must agree within the `=` tolerance (`0.5·10^-precision`) relative to the state's size, or the call stays symbolic and the REPL says why — a stiff system is refused rather than answered wrong |
| `ode([[x - x*y], [x*y - y]], [[x], [y]], t, 0, [[2], [1]], 5)` | | Lotka-Volterra |
| `ode([[t, 0], [0, -t]] * y, y, t, 0, [[1], [2]], 1)` | | `A(t)` through the single name. A right-hand side that is not a conforming column is never broadcast |

The public step and trajectory, with the method **always named** (`euler`, `rk4`, `rk45`):

| Syntax | Result | Notes |
|---|---|---|
| `odeStep(F, y, t, t0, y0, h, method)` | State at `t0 + h` (n x 1) | |
| `odeSolve(F, y, t, t0, y0, t1, h, method)` | One row per time: `[t, state...]` | `t1` is always the last row, the final interval cut short when `h` does not divide the span; at most 100 000 rows |
| `odeStep([[y]], [[y]], t, 0, [[1]], 0.1, euler)` | `[[1.1]]` | `euler`/`rk4` take exactly one step of size `h` per interval, **uncertified** — the step is yours |
| `odeSolve([[1]], [[x]], t, 0, [[0]], 1, 0.5, rk4)` | `[[0.0, 0.0], [0.5, 0.5], [1.0, 1.0]]` | |
| `rk45` | | Adaptive Dormand–Prince: `h` is the spacing of the output rows, and the steps between them hold a local error three orders below the `=` tolerance |

A name bound in the session (a control input) is a constant during the step. `euler`, `rk4` and
`rk45` are matched only in the method slot, so they stay legal variable names.
