# Changelog

Release-level history: one entry per tag, or per group of tags where the individual ones were
development markers rather than releases, or where a release changed only the documentation or
the presentation (layout, styling, branding) and no behaviour a user of the library or the REPL
could observe. Such a release is folded into the entry it follows and named in it, so it stays
findable without an entry of its own.

The per-issue record — every decision, what was rejected and why — is kept in `Done.md`, which
is maintainer-local and not published. **This file answers "should I upgrade?"; `Done.md`
answers "why is it like this?"**

Versions follow [early-semver](https://www.scala-lang.org/blog/2021/02/16/preventing-version-conflicts-with-versionscheme.html):
from 1.x upward the PATCH component promises binary compatibility and MINOR may break it.
That promise is **enforced** by [MiMa](https://github.com/lightbend-labs/mima) against the
previous release on every build, not merely declared.

**Only 3.7.1 and later reached Maven Central.** Earlier tags exist in the repository but were
never published to any artifact repository, so they are grouped by series rather than listed
one by one.

Each section's body is published verbatim as that tag's GitHub Release, extracted by
`.github/workflows/release.yml`. **Write the section before tagging**, and keep the heading in
the form `## <version>` or `## <version> - <date>` so the extractor finds it.

## 3.8.4 — unreleased

*Gathered for the next release; set its version and date before tagging.  Its number is decided at
release time from MiMa's report.*

**Answers that changed** — the same values, written as a reader would write them:

- An integral over a unit slope is no longer divided by one: `integral(exp(x), x)` is `exp(x)` (it
  was `(exp(x) / 1.0)`), likewise `sin`, `cos`, `step` and the `ln`/`atan`/`asin` forms.
- `ztrans(1, n, z)` is `z/(z - 1)`, and the transforms of `n·x[n]` come back reduced:
  `ztrans(n, n, z)` is `z/(z - 1)^2` and `ztrans(n^2, n, z)` `(z^2 + z)/(z - 1)^3`.
- `invztrans` writes a double pole's `binom(n, 1)` as `n`.
- `limit` no longer prints a wrong value as an answer: `limit((x^3 - exp(x - 1))/sin(pi*x), x, 1)` is
  `-0.63662` (it was `0`), `limit(r/(r^2 + 10*r + 25), r, -5)` is `-inf` (it was `NaN`), and
  `limit(sec(x) - tan(x), x, pi/2, -)` is `0.0` in exact mode too.  Limits whose value was lost to
  rounding or that do not exist now stay unevaluated instead of answering: `x^(-2) - (x^(-2) - 1)` and
  `ln(sin(x)/x)/x^2` at `0`, `(sin(x)/x)^(1/x^2)`, `cos(x)^(1/x^2)`, `h^(1/3)/h` at `0+`, `1/sin(x)` at `pi`,
  `tan(x)` at `pi/2`, `(-1)^(n - 1)` at `inf`.  A declined `limit` echoes the call as typed.
- `taylor`/`maclaurin` expand a quotient that is `0/0` at the centre: `maclaurin(ln(sin(x)/x), x, 4)`
  is `-x^2/6 - x^4/180` (it was a series of `0/0` coefficients), exact in exact mode.  A pole or
  `ln(0)` at the centre (`maclaurin(1/x, x, 3)`) now declines with a note instead of answering a
  series of `1/0` terms.
- `differentiable(1/(1 - x^2), x)` excludes `x = 1` again (it read `x <= 1`), and
  `differentiable(x*abs(x), x)` is `true` (it was `abs(x) != 0`).  `domain(x^(2/3), x)` is `x >= 0`
  (it was `true`, though the library answers a complex value for a negative `x`), and
  `domain(x^(-1/3), x)` is `x > 0`.
- In exact mode the hyperbolic and reciprocal functions (`sinh` … `atanh`, `sec`, `csc`, `cot`, `sech`,
  `csch`, `coth`) carry the working precision, so identities through them compare equal:
  `sec(1)^2 == 1/cos(1)^2` and `acosh(2) == ln(2 + 3^(1/2))` are `true` (they were `false`).
- A derivative in a variable bound to a formula is taken before the formula is substituted: with
  `x := cos(t)`, `y := sin(t)`, `derive(x^2 + y^2, x)` is `2*cos(t)` and `grad(x^2 + y^2, x, y)` is
  `[[2*cos(t)], [2*sin(t)]]` (both were `0`).  When the variable does not appear in the expression the
  chain rule still applies; `derive(h, h)` for a definition `h` is `1`, where it was refused.
- The definite integral no longer answers across a pole or with a wrong number over a long range:
  `integral(1/(x - 2), x, 0, 3)` and `integral(sec(x)^2, x, -pi, pi)` stay unevaluated (they were
  `2.32985` and `2.2E30`), `integral(1/x^2, x, 1, 10000)` is `0.9999` (it was `3.49848`) and
  `integral(cos(x), x, 0, 1000)` is `sin(1000)`.  Where the antiderivative is smooth on the interval
  the answer is `F(b) - F(a)`, exact in exact mode (`integral(x^2, x, 0, 1)` is `1/3`); otherwise a
  certified adaptive quadrature answers to the precision asked, or declines.  New budget:
  `Limit.MaxQuadratureEvaluations`.
- `solve` of a transcendental equation keeps the roots nearest 0: `solve(tan(x) = 0, x)` binds `0`, `±π`,
  … (it bound eight roots near `-90`).  A periodic equation, or one with more roots than are listed, now
  carries a note saying the list is partial.
- An exact surd whose terms nearly cancel reads back right: `(10^32 + 10^16)^(1/2) - 10^16` in exact mode
  is `0.5` (it was `0.25`).  The cause was spire's `Rational.toDouble`, which misrounds a wide rational;
  the exact linear programming and Gröbner answers read through it too and are corrected with it.
- `exact on` reaches every constant a computation invents: `derive(atan(x), x)` is `(1 / (1 + (x ^ 2)))`,
  `integral(y, y)` `((y ^ 2) / 2)`, `div([[x], [y], [z]], x, y, z)` `3` and an iterated integral with a
  variable limit exact (all were decimals).  An answer that is still a decimal under `exact on` says so
  in a note.  A definite integral with a symbolic limit now answers `F(b) - F(a)` when the antiderivative
  is smooth on the whole line.  New, additive: `Environment.exactMode`, `withExactMode`.
- In exact mode a power with a long decimal exponent answers promptly: `2^(1.00001)` took 38.6 s and
  `sum(1/k^(1.000001), k, 1, 1000)` never returned; a sum of 100 such terms now takes about a second.
- **New: a definition with a parameter list**, `f(x) := x^3 - 2*x`, is applied by `f(2)`, `f(x + h)` or
  `(f(b) - f(a))/(b - a)`; `g(x, y) := …` takes several.  A definition without a list is unchanged:
  `f(2)` stays a product, with the note that says so.  `:save` writes the list.
- `limit` answers what sampling cannot certify: a free parameter (`limit(((x + h)^2 - x^2)/h, h, 0)` is
  `2x`), growth at infinity (`n/2^n` is `0`, `(1 + 2/x)^x` is `e^2`, `x^2 - 3x + 4` is `inf`), infinite one-sided
  limits (`ln(t)` at `0+` is `-inf`) and the cases F_0276 had to decline (`(sin(x)/x)^(1/x^2)` at `0` is
  `e^(-1/6)`).  A generic answer notes what it assumed: `limit(x/(x - a), x, 2)` assumes `2 - a != 0`.
- Improper integrals answer by the limits of their antiderivative: `integral(exp(-x), x, 0, inf)` is `1`,
  `integral(exp(-x^2), x, -inf, inf)` is `sqrt(pi)`, `integral(ln(x), x, 0, 1)` is `-1`; a divergent one
  answers its infinity (`integral(1/x, x, 0, 1)` is `inf`, where it stayed unevaluated).
- `integral` closes the standard forms it left unevaluated: `1/(7*x^(1/2))`, `2*x*x^(1/2)`, `1/x^(3/2)`,
  `(x^2 - 2*x + 2)^2`, `exp(-x)*sin(x)`, `exp(x/2)*cos(2*x)`, `ln(x)/x^2`, `1/(x*ln(x)^2)`,
  `x*(1 - x)^(1/2)`, `x^3*(1 + x^2)^(1/2)`, `1/(x^2 + 4)^(3/2)`, `1/(x^2*(9 + x^2)^(1/2))`,
  `x^(1/2)/(x^2 + x)`, `sin(x)^5*cos(x)^8`, `x*sin(x)*cos(x)` and `cos(ln(x))`, exact in exact mode where
  the answer is algebraic.  Only integrals that stayed unevaluated are affected.
- A vector operator sees later bindings of its coordinates: after `H := hessian(f, x, y)`, `x := 1` and
  `y := 1`, `H` is `[[2.0, 2.0], [2.0, 6.0]]` and `eigen(H)` answers, as `det(H)` already did (`H` kept `x`
  and `y`).  `grad`, `curl` and `jacobian` alike; `div(grad(f, x, y), x, y)` still differentiates first.
- **`pi` and `e` are symbols now**, and two constants are new: `eulergamma` (`γ`) and `goldenratio` (`φ`).  A
  formula, a definition echo and `:save` write `pi` (`g := sin(pi*y/2)` echoed `sin(((3.14159 * y) / 2))`).
  Outside exact mode every value is what it was.  In exact mode `pi/2` stays `pi/2`, `sin(pi/6)` is `1/2`,
  `cos(pi/2)` is `0` (it was `2.0670E-43`), `tabulate(sin(k*pi), k, 1, 4)` is `[[0, 0, 0, 0]]`, `tan(pi/2)`
  stays undefined, `goldenratio` is the exact `1/2 + √5/2`, and where no exact rule applies (`cos(pi/7)`,
  `pi > 3`) the constant is read at the working precision as before.  LaTeX renders `\pi`.  New, additive:
  `core._Constant`, `ConstantKind`, `foldConstants`, `evalNumeric`, `eulerGammaAt`, `Parser.namedConstant`.
  `gamma` and `phi` stay free variable names.
- A `sum` with a symbolic bound closes for a polynomial or geometric summand: `sum(k, k, 1, n)` is
  `n^2/2 + n/2`, `sum(2^k, k, 0, n)` is `2^(n + 1) - 1`, and `limit(sum((k/n)^2/n, k, 1, n), n, inf)` is `1/3`
  (all stayed as written).  Other summands, and `product`, stay unevaluated.
- `simplify` collects like terms, applies `sin(u)^2 + cos(u)^2 = 1` and cancels a common factor:
  `simplify cos(t)^2 + sin(t)^2` is `1.0`, `simplify det(jacobian([[r*cos(t)], [r*sin(t)]], r, t))` is `r`,
  `simplify ((x + h)^2 - x^2)/h` has no `/h`, the spherical `div([[r], [0], [0]], r, t, p, spherical)` is `3.0`.
  A `derive` answer gets the same: `derive(sin(x)^2 + cos(x)^2, x)` is `0.0`.  Nothing else changes form.
- `solve` inverts `exp`, `ln`, `log` and a power of a constant base around the unknown, so a root outside the
  numeric scan is found: `solve(exp(-t/1000) = 1/2, t)` is `693.14718` (it stayed unevaluated), and
  `solve(exp(t) = -1, t)` has no root.  A polynomial inequality of any degree is answered by its sign chart:
  `solve(x^2*(2*x - 9) < 0, x)` is `x < 0 or (x > 0 and x < 4.5)`, and `<=`/`>=` include the roots.
- `solve` answers an inequality on an absolute value: `solve(abs(x - 1) < 1/20, x)` is the band
  `0.95 < x < 1.05`, `abs(u) > c` the two tails, `abs(u) < abs(w)` by squaring; a negative bound decides it
  outright, a bound of unknown sign stays unevaluated.
- Fewer empty notes in exact mode: an infinite answer (`limit(x^2, x, inf)`) is no longer called "a decimal
  approximation", and the `rationalize` offer is not made when it would only restate an unevaluated call.
  `div([[x], [y], [z]]/r, x, y, z)` and `div(2*[[x], [y], [z]], x, y, z)` read the scaled column as a field.
- A fractional power of a negative number is the principal complex value in every tier, as it already was
  for evaluation: `limit(x^(1/3), x, -8)` is `1 + 1.732i` and `integral(x^(1/3), x, -8, -1)` its integral (both
  stayed unevaluated with a note calling `-8` undefined), `limit(ln(x), x, -1)` is `i*pi`.  The note now says
  "outside its real domain".  LaTeX keeps `x^{1/3}` unless the base cannot be negative, since `\sqrt[3]{x}`
  reads as the real root.
- `derive` of a root is written `(1/3)*h^(-2/3)`: an exponent free of the variable takes the power rule however
  it is typed (`derive(h^(1/3), h)` was `h^(1/3)*(0*ln(h) + (1/3)/h)`, which read `0` at `h = 0`).

## 3.8.3 — 2026-11-01

**New in the library and at the prompt**

- **Optimisation, a new package.** `stationary` lists every stationary point of a polynomial
  objective (elimination with case splits, an exact Gröbner basis where that gives up) and the
  REPL classifies each as a local minimum, maximum or saddle; `convex` proves or refutes
  convexity; `lagrange` and `kkt` handle equality and inequality constraints. Numerically:
  `minimize` (`gd`, `newton`, `bfgs`, `pbfgs`, the method always named), `qp` for convex
  quadratic programs (positive definite or semidefinite), `lsqfit` for nonlinear least squares and
  curve fits, `nlp` for constrained nonlinear programs (`sqp`, `auglag`), and `lp`, an exact linear
  program solver. Every numeric answer is certified; an uncertified one is declined with the
  reason as a note.
- **Control and MPC.** `mpcMatrices` (prediction matrices), `linearize` (a nonlinear model at an
  operating point), `arx` / `arx2tf` / `arx2ss` (identification from data, several inputs, and
  several coupled outputs), `dlsim` (discrete simulation), `dare`, `dlqr`, `dlyap`, `kalmanGain`,
  `kalmanStep` (the stabilising solution only), and at the prompt `tfpoles`, `tfzeros`, `isStable`,
  `isStableDiscrete`, `stateSpace`, `c2d` (now also `foh`, `impulse`, `matched`, `forward`,
  `backward`, and prewarped Tustin), `controllable`, `observable`, `ctrb`, `obsv`, `routhTable` and
  `acker`. The browser draws a root locus (`rlocus`).
- **Matrices.** `chol`, block matrices (`hcat`, `vcat`, `blkdiag`, `repmat`, `kron`, `ones`,
  `submatrix`), `svd`, `pinv`, `rank`, `cond`, `nullspace`, `orth`, `norm` (1, 2, inf, `fro` or any
  `p` for a vector), `dot`, `cross`, `trace` and `size`. `eigen` now answers random dense matrices
  (a Francis double-shift QR iteration replaced one that declined most of them from 3×3 up).
- **Statistics and probability.** Bayesian inference (`betadist`, `gammadist`, `bayes`,
  `posterior`), seeded sampling (`draw`, reproducible across the JVM and the browser) and `prbs`;
  ridge and weighted regression; recursive least squares (`rls`); `median`, `sort` and sample
  `quantile`; two-sample, proportion, variance and independence tests and `anova`; the
  geometric, hypergeometric and F families; `bincounts`, and `hist` in the browser.
- **Calculus, equations and more.** `ode` solves linear systems by the matrix exponential and
  integrates time-varying and nonlinear systems with a certified error, and `odeStep` / `odeSolve`
  expose the integrator; Taylor coefficients come from power-series arithmetic (faster, and exact
  on exact input); `maximum`, `minimum`, `max`, `min`, `clamp`, `softplus`; interval arithmetic
  with rigorous bounds, `[lo, hi]`; `dft`, `idft`, `conv`; `cnf`, `dnf` and the defuzzification
  methods at the prompt; `solveSystem` with more equations than unknowns, the equations also
  written as a column `[[eq1], [eq2]]`, every refusal with its reason.
- **Exact arithmetic reaches further.** Calculus stays exact on exact input and on names bound to
  exact values; `lu` (a singular matrix included), `A^n`, `jordan` with rational eigenvalues,
  Sylvester and Lyapunov equations, `solveSystem` and `stationary`'s rational points are exact; in
  exact mode two values are equal when they agree to the working precision, with a note saying so;
  square roots stay exact surds, shown `√13` and typed back with `√`; `rationalize(x)` makes a
  decimal exact on request, and the REPL offers it when an exact input came back as decimals that
  look exact.
- **Library API.** Every work and size budget is configurable through `Environment.limits`
  (`env.withLimit(Limit.X, n)`); the REPL and the browser keep the defaults. `compile` takes
  several variables or a whole matrix. `cli.ShareEncoding`, the run-link format, is public, and
  `Session.figure` draws the drawing commands as an SVG image.
- **REPL and browser.** Under `pretty on` answers use the fewest brackets and a binary minus
  (`4 - k*k`); a definition whose formula already computes echoes its current value on a `= …`
  line, and an exact matrix assignment echoes the matrix; the browser adds `curve`, `arrows`,
  `polygon` and layered figures (`;`), and enforces a Content-Security-Policy.

**Answers that changed** — re-check results that depended on these:

- `fourier` is the **two-sided** transform and declines where the integral diverges
  (`fourier(exp(-t), t, w)` declines; the old one-sided answer is `fourierCausal`).
- `eigen`, `eig` and the numeric `jordan` list eigenvalues by real part, then imaginary part.
- `expand` writes a one-variable polynomial by descending degree and divides a sum by a numeric
  divisor term by term.
- `minimize` answers only a minimiser, never a maximum or saddle; `stationary` calls an extremum
  local.
- A numeric `limit` that only seemed to converge declines (`limit(sin(x)/1e7, x, inf)`).
- `jacobian` and `hessian` are Cartesian only, and `grad`/`hessian` refuse a vector argument;
  `eig` refuses a defective matrix instead of returning dependent eigenvectors.
- `differentiable(step(x), x)` is `x != 0` (it was `false`).
- A computed fraction, imaginary or negative number in a divisor or a power is printed bracketed,
  so a printed answer types back as itself; exact square roots print `√2` where they printed
  decimals (`:save` writes them as `(2^(1/2))`), and `(2^(1/2))^2 == 2` is exactly true, so its
  working-precision note no longer appears; `rationalize(x)` defaults to the digits that give the
  small fraction; a `qr` of dependent columns says why it stays unevaluated.
- `solveSystem` declines an over-determined numeric system whose columns are nearly dependent.
- Very large exact numbers are now held to measured budgets: an exact `+ − × ÷` or matrix power
  past them stays unevaluated instead of running for minutes — a quotient of two large integers
  included, and the exact `mean`, `variance`, `det`, `inv`, `lu` and `solveSystem` over a long or
  large-denominator input.  A sum of two large integers, refused before, is now computed.
- `limit` looks at the scales an expression is written with: `limit(exp(-1e-20/x), x, 0, +)` is `0`
  (it was `1.0`), and limits whose two sides disagree below the old sample points, or whose value
  shows only past the old samples at infinity, now decline instead of answering wrongly.
- `minimize` refuses a maximum hidden below its old witness step, and answers steep, flat or shifted
  objectives it declined (`1e40*(x-1)^2`, `(x-1)^2 - 1e101`).  A `laurent` expansion no longer puts a
  pole at a regular point; `solveSystem` solves a system with two radicals in any units, and declines,
  rather than fits untested, a large over-determined one whose rank it cannot test.
- In exact mode `limit` at a surd point, a limit that is a surd, sums and products of exact limits at
  infinity, and `laplace`/`invlaplace` with a shifted step keep their exact answers.
- In exact mode the working precision keeps its digits for a small value: `exp(-100)` is
  `3.7201E-44` (it was `0`) and `ln(1 + 10^-40)` is `1.0E-40`.  As a consequence `sin(pi)` shows the
  sine of `pi`'s approximation, a number near `10^-43`, where it showed `0`; `sin(pi) == 0` is still
  `true`, with its working-precision note.

**Fixes.** Dozens of thresholds that silently assumed unit-sized input are now relative to the
problem's own scale (rank, pivots, roots, degrees, certificates), so the same question in other
units gets the same answer. The 2026-10-23 audit's edge cases — wrong answers and wrong refusals
across limits, inequalities, transforms, statistics and the math editor — are fixed. `:save` and
share links write every value at full precision and reload it in its own tier. Named exact
matrices multiply exactly, and a matrix is never used as a scale factor. Nothing deeper than the
browser can walk is accepted or built. A large matrix literal parses several times faster.
`hist` on a sample spanning the whole number range draws its bins, or says why it cannot; the
browser's `:save`/`:load` answer when storage is blocked instead of failing silently.

**Supply chain.** MathLive is at 0.111.3, past 0.111.2's security release (the math editor's
virtual keyboard accepts messages from its own origin only), the floor its advisories require. The release
build overrides BouncyCastle to 1.86, clear of the 1.78.1 advisories. Workflows hold no more
permission or stored credential than their steps need. The architecture diagram is exported as
GraphML for Gephi and yEd under a standing check, and an eighth guard keeps the grammar's words
reserved; three guards were tightened where they could be passed (HTML case, titled links, string
literals).

## 3.8.2 — 2026-10-06

*A correctness fix. Upgrade.*

- **A leading minus now negates only the first term.** Since the grammar's first release, a sign
  at the start of an expression applied to the whole sum: `-1 - 1` evaluated to `0`, `-x + y` to
  `-(x + y)`, and `-2*x - 3` to `-2x + 3`. It now reads as written everywhere an expression is
  parsed — the REPL, the browser, `Parser.parse`, function arguments, equations and inequalities.
  `solve(-x + 1 = 0, x)` answers `x = 1` (it answered `x = -1`), and `solve(-x^2 + 4 > 0, x)`
  answers `-2 < x < 2` (it answered `false`). An expression with a single leading term — `-x`,
  `-3k`, `-(a + b)`, `-2^2` — parses exactly as before.

> **Saved sessions and share links can change meaning, and that includes ones Leonardo wrote.**
> A line such as `f := -x + y` in a `:save`d script or a shared link was evaluated wrongly by
> every earlier release and is evaluated correctly by this one. The library's own output was
> affected too: a sum beginning with a negative number — which `simplify`, `consolidate` and the
> exact tier can produce — prints as `(-3.0 + x)`, and earlier releases read that back as
> `-(3 + x)`. Re-check any result computed from such a line.

> **Binary compatibility.** Both artifacts are binary-compatible with 3.8.1; MiMa finds no change.

## 3.8.0 – 3.8.1 — 2026-10-02

*On Maven Central.* The browser release: Leonardo runs in a web page, renders LaTeX, and comes
with a course of lessons. 3.8.1 changed only the browser page's layout, described in the first
entry below.

- **Browser REPL** — the library and the REPL are cross-built for Scala.js, and the full REPL
  runs at [`/app`](https://lycogrypho.github.io/Leonardo/app/) with nothing to install: plotting
  (`plot`, `points`, `bode`, `nyquist`), shareable links that carry a whole session in the URL, a
  settings panel, and `:save` / `:load` backed by browser storage. Every GitHub Release now
  attaches the app as `leonardo-app-<version>.zip`, which runs from any folder. The Maven
  artifacts remain JVM-only. *Since 3.8.1:* on a wide window a plot opens in a column beside
  the REPL rather than below it (on a narrow window, or a browser without `:has()` support, it
  stays below), and the page header shows the project banner, as the documentation site does,
  in place of a text title.
- **LaTeX** — a new `latex` package renders any expression as math-mode LaTeX (`ToLatex`). In the
  REPL, `latex on` offers each result's LaTeX on a second channel, which the browser typesets
  beside the answer; the printed text itself never changes.
- **Visual math editor** — in the browser, a formula can be entered in a math field. Integrals,
  derivatives, limits, sums, products, binomials and `|x|` convert into the grammar, and anything
  the grammar does not know is refused rather than guessed.
- **New in the language** — `abs` (absolute value and complex modulus, exact on exact input);
  finite `sum` / `product`; integral and `defuzz` limits that accept any expression
  (`integral(sin(x), x, 0, 2*pi)` used to be a parse error); and `control.frequencyResponse` /
  `nyquistSweep`, log-spaced sweeps with the phase unwrapped.
- **Lessons** — twenty lessons in two branches, *at the prompt* and *in your program*, every
  example executed when the site is built. Each transcript carries a link that opens the browser
  REPL with that session already loaded.
- **Fixes.** Saved sessions and share links now reproduce definitions exactly — a constant inside
  a definition was written at five decimals, and a long exact rational as a decimal, so a
  reloaded session could compute different numbers. `simplify` and `expand` reduce derivatives
  and integrals, as plain evaluation always did. Session precision and `pretty` now reach
  composite results, not only bare values, and `pretty` and `latex` no longer hide each other. A
  plot reports the samples it had to drop, and refuses a grid that would miss an expression
  defined only at whole numbers. `exact precision` is capped at 1000 digits. A call to a function
  the grammar does not know, such as `sqrt(x)`, is now flagged instead of being read silently as
  a product.
- **Supply chain and maintenance.** Repository guards rewritten in Scala, plus a new one that
  fails the build on a broken documentation link; JVM dependencies covered by security-advisory
  alerts; `scala-parser-combinators` held at 2.4.0, because 2.5.0 would raise the minimum JDK
  from 8 to 17 for every consumer; Scala 3.3 LTS, JLine 3 and sbt 1 pinned against unreviewed
  major upgrades.

> **Binary compatibility.** `leonardo` is binary-compatible with 3.7.2 — MiMa finds no change. The
> REPL artifact `leonardo-repl` is not, in exactly one place: splitting its source for the
> Scala.js build removed the compiler-generated class `cli.Repl$package`. No source change is
> needed, but code compiled against 3.7.2's REPL artifact must be recompiled — which is why this
> is 3.8.0 rather than 3.7.3, chosen from MiMa's report as 3.7.2's note promised. 3.8.1 changed
> no library or REPL source, so both artifacts are binary-compatible with 3.8.0.

## 3.7.2 — 2026-09-13

*On Maven Central.* The control-theory release.

- **`control` package** — transfer-function algebra (`series`, `parallel`, negative `feedback`),
  poles, zeros, stability, step and impulse response, Bode and Nyquist, state space with
  controllability and observability, and continuous-to-discrete conversion (ZOH, Tustin).
  A transfer function is an ordinary `Ratio`, not a new type, so `simplify`, `derive`,
  `substitute` and the exact tier all apply to one unchanged.
- **`expm(A)`** — matrix exponential by scaling-and-squaring with a Padé approximant, correct
  for defective matrices where the eigen route has no basis to work in.
- **One-sided z-transform** — `ztrans` / `invztrans`, with the inverse by partial fractions.
- **Fixes.** Rank is decided by QR rather than a Gram determinant, which was scale-dependent
  and could report a controllable plant as uncontrollable; generated variable names in the ZOH
  path are now capture-safe; a dead division-by-zero guard in `rationalCoeffs` was reached and
  repaired; `routh` released as a usable variable name.
- **Removed:** the unused `expr` package and its `EvalResult` enum. Public API, so a binary
  break — but nothing referenced it, and it shipped in a patch release. See the note below.
- **Supply chain and encoding.** Every workflow action pinned to a commit SHA and *enforced* by
  a CI guard; source-encoding guards; MiMa wired into CI.
- Documentation reviewed across every page; the architecture diagram is now pan- and zoomable.

> **Note on the version number.** Removing `expr` was a binary break and should have made this
> a minor release. Nothing outside the library ever referenced `EvalResult`, so no consumer can
> observe it, and 3.7.2 stands. The practice that follows: the number is chosen at release time
> from what MiMa reports, never declared in advance.

## 3.7.1 — 2026-09-08

*First release on Maven Central.*

- Published as `it.grypho:leonardo` and `it.grypho:leonardo-repl` for Scala 3.3 LTS.
- README restructured around what the project is and how to depend on it.

## 3.7.0 — 2026-09-06

*Not published — the release pipeline itself.*

- Split into two artifacts so that **a library consumer never resolves JLine**: `leonardo` is
  everything except the REPL, `leonardo-repl` is the REPL and the only thing needing a terminal
  library.
- Relicensed **GPL-3 → Apache-2.0**; moved to the `it.grypho` namespace; releases driven by a
  git tag alone; JitPack dropped.

## 3.6.0 – 3.6.3

- **Vector calculus** — `grad`, `div`, `curl`, `laplacian`, `jacobian`, `hessian` over an
  explicit ordered coordinate tuple, in Cartesian, cylindrical and spherical coordinates (both
  the physics and the mathematics angle conventions).
- **Numeric sequences** — `fib`, `lucas`, `pell`, `jacobsthal`, `binom`, `catalan`, `harmonic`,
  and a generic `tabulate` that gives every function a tabulated form.

## 3.5.0 – 3.5.6

- **The integration programme.** A data-driven table of integrals over a pattern-rewrite
  engine, plus non-linear u-substitution, trigonometric and hyperbolic substitution,
  the Weierstrass half-angle substitution, full partial fractions with repeated and complex
  roots, and reduction formulas for `tan`/`sec`/`csc`/`cot` powers.
- **Twelve new functions** — the hyperbolics and their inverses, and the reciprocal trig
  functions `sec`, `csc`, `cot` and their hyperbolic counterparts.
- **Special integral functions** — `Si`, `Ci`, `Ei`, `li`, `fresnelS`, `fresnelC`.
- Repeated poles in the inverse Laplace transform.

## 3.0.0 – 3.4.0

- **Domain analysis** — `domain`, `differentiable`, `singularities`.
- **Inequality solving** — `solve(x^2 - 4 > 0, x)` answers `x < -2 or x > 2`.
- **Laurent series**, **base conversion** (`0xFF`, `0b1011`, balanced ternary), **separable
  ODEs**, and the rewrite engine the integral table is built on.

## 1.1.8 – 2.11.6

The bulk of the library, developed before any of it was distributed:

- **Exact arithmetic** — a rational tier with arbitrary-precision transcendentals, exact
  matrices, and a benchmarked gcd policy.
- **Logic** — boolean, three-valued (Kleene) and fuzzy, over one shared rule table, plus a
  symmetric-ternary encoding.
- **Probability and statistics** — distributions as first-class values, expectation and
  variance by a linearity rule table, descriptive statistics, regression by QR, and elementary
  inference.
- **Series** — Taylor, Maclaurin, Fourier and Padé.
- **Special functions** — the factorial and gamma family, then the analytic tier (`erf`,
  incomplete gamma and beta, `digamma`).
- **Comparison operators**, **ODEs** (closed-form and Runge–Kutta), **integration by parts**,
  **reduction formulas** and **partial fractions**.
- Complete ScalaDoc, and the documentation site on GitHub Pages.
