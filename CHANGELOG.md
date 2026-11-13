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
release time from MiMa's report — **except that the JDK floor below makes it at least a minor release
(3.9.0)**, a break MiMa cannot see.*

**Requires JDK 25.**  `leonardo` and `leonardo-repl` are now compiled for Java 25 (class-file major 69);
until this release they ran on Java 8.  A consumer on an older JDK stays on 3.8.x.  The change comes
with the MCP server module, whose SDK needs a modern JDK, and is applied to every published module at
once so they keep one floor.  `scala-parser-combinators` moves from 2.4.0 to 2.5.0, held back until now
only by the old floor.

**New: an MCP server, `it.grypho:leonardo-mcp`** (a third published module, plus a runnable
`leonardo-mcp-<version>.jar` on each GitHub Release).  An AI assistant that speaks the Model Context
Protocol — Claude Desktop, Claude Code and others — can call Leonardo's REPL as a tool: `evaluate(line)`
on a per-connection session, the typed tools `simplify`, `derive`, `integrate`, `solve`, `limit`, and
`scala_value`, which returns a result as Scala source constructing it.  The grammar and the session's
script are served as resources; each request is bounded by the work budgets and a wall-clock timeout,
after which the session continues from its state before the request; while four such abandoned
computations are still running, a new request is refused until one finishes.  See the MCP page of the site.
The library and the REPL do not depend on it.  Additive: `Session.lastResult`.

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
- `minimize` no longer declines "unbounded below" from a start where the objective is `0`:
  `minimize(x^2 - 2*x, [[x]], [[0]], bfgs)` is `[[1]]` by every method (3.8.3 read any decrease below
  zero there as unbounded).
- `stationary` of a constant written through a node that does not evaluate, such as
  `stationary(dot(0, 0), t)`, stays unevaluated; it answered `false`, "no stationary point", where every
  point is one.
- Under `exact on` an inequality's solution set and a domain have exact bounds: `solve(x^2 > 4, x)` is
  `x < -2 or x > 2` (it was `-2.0`, `2.0`, with no note), `domain(ln(x - 1/3), x)` is `x > 1/3` (it was
  `0.33333`), and a quadratic's irrational bound is a surd.  A bound with no exact form stays a decimal.
- Under `exact on`, exact roots that lie close together are no longer merged: `solve((x - 1)*(x - 1 - 1/10^6) > 0, x)`
  is `x < 1 or x > 1000001/1000000` (it was `x != 1`), `domain` keeps two close poles apart and `singularities`
  reports them as two simple poles.  An inequality or a domain over a rational function with exact
  coefficients is now decided by an exact sign chart: the real roots are counted and isolated with no
  floating point, the rational ones and the surds of a remaining quadratic factor exact
  (`singularities(1/((x - 1/3)*(x^2 - 2)), x)` is exact throughout), any other read to full `Double`
  precision.  A non-strict rational inequality is answered there too: `solve((x - 1)^2/(x - 2) >= 0, x)` is
  `x = 1 or x > 2`.  New budget `Limit.MaxExactRootWork`; past it, and outside exact mode, the answers are
  as before.
- `limit` at infinity through a function no longer answers a call that did not evaluate or a pole read at
  its rounding: `limit(digamma(1/x), x, inf)` answered `digamma(0.0)` and `limit(sec(1/x + pi/2), x, inf)`
  `1.6E16`; both now stay unevaluated or answer the limit.
- `limit(floor(x/0.1), x, 0.3)` stays unevaluated, the two sides being `2` and `3`; it answered `2.0`,
  because `0.3/0.1` is `2.9999999999999996` in floating point and was not read as a jump.
- `minimize`, `lsqfit`, `nlp` and `kkt` evaluate their expressions first, as `stationary` does, so an
  objective written through `dot` or `derive` is minimised: `minimize(dot([[x]], [[x]]) - 2*x, [[x]], [[0]], bfgs)`
  is `[[1]]` (it declined as "not a real number").  A session binding of a variable's own name never folds it.
- Under `exact on` the answer of a numeric method (`minimize`, `lsqfit`, `nlp`, `qp`, `ode`) carries the
  "decimal approximation" note, no longer an offer to `rationalize` its iterates.
- The limit algebra applies a function's value at the limit of its argument only to functions declared
  continuous where they are defined, so a limit through a function with no such declaration (a
  functional, a sequence) is declined rather than assumed.
- **New: `nsolve(F, [[x], [y]], x0[, lb, ub])`**, a root of a square nonlinear system by Powell's dogleg trust
  region, answered only when every equation vanishes against the size of its own terms and the Jacobian is
  nonsingular there; a minimum of the residual that is not a root is declined, with the reason in a note.
  In a program, `nsolve` over expressions or over numeric callbacks (the system, its Jacobian and each
  equation's scale).
- `derive` and `integral` of a `taylor`, `maclaurin`, `pade` or `laurent` call compute the series first
  (they echoed the call; only `simplify` answered).
- `stationary`, `convex` and `lagrange` evaluate an objective written through `dot`, `derive` or another
  computing node before analysing it: `stationary(dot(derive(q, t), derive(q, t)), t)` answers `t = 0`
  (it echoed itself).
- In exact mode a based literal is exact: `0b1010 + 0b0110` is `16` (it was `16.0` with a `rationalize`
  note) and `tobase(10, 2)` carries no note.  New, additive: `_Based.ofExact`, `_Based.exact`,
  `Environment.withoutBindings`.
- A logical connective with one known operand folds it on either side: with `p := true`, `p and q` is
  `q` (it was `(true and q)`), `q implies true` is `true`, `p xor q` is `not q`.
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
- **New: `floor(x)`, `ceil(x)`, `round(x)` and `mod(a, b)`.**  Exact on exact input (`floor(7/2)` is `3`; a
  surd is decided by integer arithmetic, so `floor((10^80 - 1)^(1/2))` is `10^40 - 1` although its
  30-digit reading is `10^40`), element-wise on a matrix, a complex or interval argument left as written.
  `round` sends a tie **away from zero** (`round(2.5)` is `3`, `round(-2.5)` is `-3`; Python and numpy go half
  to even).  `mod` takes the sign of the divisor, `mod(-7, 3)` is `2`, and on doubles is the exact
  remainder rather than `a - b*floor(a/b)`, which loses every digit past `2^53`; a zero divisor stays as
  written.  `derive` is zero between the jumps and `differentiable` excludes them, the answer staying
  symbolic as for `tan`.  A definite integral of a linear argument's staircase is split at its jumps
  (`integral(floor(x), x, 0, 3)` is `3`, exact on exact input; new budget `Limit.MaxStaircasePieces`), and a
  two-sided `limit` at a jump declines while each one-sided limit answers.  In exact mode a based literal
  stays exact in a matrix, negated and in `binom`.  LaTeX: `\lfloor x \rfloor`, `\lceil x \rceil`, `a \bmod b`.
- **New: `a iff b` and `equivalent(f, g)`.**  `iff` is the biconditional, the loosest connective, and does
  not chain (`a iff b iff c` is a parse error: it is associative classically, not in a many-valued
  logic); it is exactly `not (a xor b)` in every semantics, and `cnf`/`dnf`/`simplify` read it.
  `equivalent(f, g)` is `true` when two formulas agree on every assignment of their free names as booleans,
  and `false` with the first differing row in a note; past 16 names, or on a row that is no boolean, it
  stays as written with the reason.  It is not `==`, which compares values.
- `simplify` combines and applies De Morgan where that shrinks the formula: `(p and q) or (p and not q)` is
  `p`, `(p implies q) and p` is `p and q`, `not (not p and not q)` is `p or q`, and `not (a iff b)` is
  `a xor b`.  The first two hold only in classical logic and are gated on crisp operands.
- **New: `mindnf(f)` and `mincnf(f)`**, a minimal sum of products and product of sums by Quine–McCluskey
  and an exact minimum cover: the fewest terms, then the fewest literals.  `mindnf((p or q) and (not p or r))`
  is `(p and r) or (not p and q)`, where `dnf` keeps a third, redundant term.  A cyclic cover has several
  minimal forms: one is answered, the first in a fixed order that is the same on the JVM and in the browser,
  and a note says how many exist.  A relation is an atom, as for `cnf`.  The search is bounded by two new
  budgets, `Limit.MaxMinimiseVars` (15, the most a cube packs into) and `Limit.MaxMinimiseWork`
  (5 million units, about 1.5 s at worst in a browser); past either, or for a graded value, the call stays as
  written with the reason.  In a program, `minimalForm(e, conjunctive)` returns the form and its count.
- Under `pretty on` a formula is written with the fewest brackets the precedence allows, `p and q or r`
  rather than `((p and q) or r)`.  `truth f xor g` heads its table with the formula as typed, `(f xor g)`,
  not the two definitions in full, and a `?` that comes from a relation (`truth x > 0 or y`) says so in a
  note.
- **Eight words are reserved now**: `floor`, `ceil`, `round`, `mod`, `iff`, `equivalent`, `mindnf` and
  `mincnf`.  A saved session
  that used one as a variable name must rename it; `:load` reports it as a reserved word.

**New in the library** (additive, no prompt form; requested by an MPC application):

- **Numeric MPC prediction matrices**: `mpcPrediction(A, B, C, Np, Nc, limits)` gives `Φ`, `Γ` as dense
  matrices (the last move held past `Nc`, as `mpcMatrices`), and `mpcIncrementalPrediction` the
  `Δu` form `Φ`, `Ψ`, `Θ`, with `y = Φ·x + Ψ·u(k−1) + Θ·Δu`.
- **The MPC quadratic programme**: `mpcQp(prediction, Q, R, reference, x, uPrev, bounds, limits)`
  assembles `H`, `f` and the constraint rows for input, rate and output bounds in either formulation,
  output bounds optionally soft (one slack per output, linear and quadratic penalty), and `solve`
  answers through `qpSolve`, the first move and the slacks unpacked.
- **Offset-free MPC**: `augmentDisturbance(A, B, C, Bd, Cd, env)` and the helpers
  `inputDisturbanceModel` / `outputDisturbanceModel` build the plant augmented with integrating
  disturbances, declining a non-detectable augmentation; `disturbanceObserver` gives its Kalman gain
  and `steadyStateTarget` the target `(x_s, u_s)` for a setpoint and a disturbance estimate, the closest
  reachable one under input bounds.
- **ARX order selection**: `arxOrderSelection(u, y, nas, nbs, nks, criterion, validation, env)` scores
  a grid of `(na, nb, nk)` by AIC, BIC or FPE (always named), on a validation split if given, every
  candidate on the same samples; an unexcited candidate is skipped and reported.  New budget:
  `Limit.MaxArxOrderWork`.
- **MPC weights** (overload `mpcQp(prediction, MpcWeights(…), …)`): per-step output and move weights, a
  terminal cost on `x(k+Np)` (`mpcTerminalPrediction`, `MpcTerminal`) — with `P − CᵀQC`, `P` from `dlqr`,
  the unconstrained MPC is the LQR law — and an input target `u_s` for the absolute form, whose `R`
  otherwise pulls the inputs to zero and offsets tracking.
- **NMSS realisations of an ARX model** with a measured state, `arx2nmss` and `arx2nmssIncremental`
  (input `Δu`, an integrator on every output), so an MPC on them needs no observer; and **`minreal`**,
  which reduces any realisation to its controllable and observable part, certified by its Markov
  parameters.
- **An MPC on a model whose input is an increment**: `mpcAccumulatedPrediction(A, B, C, Np, Nc, limits)`
  (and `mpcAccumulatedTerminalPrediction`) for a model with `Δu` as its input and the past input in its
  state, `arx2nmssIncremental`'s included.  `mpcQp` then reads `MpcBounds.u` as bounds on the actuator
  **position** `u(k−1) + Δu(k) + … + Δu(k+j)` and `MpcBounds.du` as bounds on the moves, and `solve`
  returns the input to apply.  The increments are zero past `Nc`, so the input is held: passing the same
  model to `mpcPrediction` instead holds the last *increment*, a ramp, unless `Nc = Np`.
- **A cost on the actuator position when the moves are increments**: `MpcWeights.positionTarget`, an
  `MpcPositionTarget(S, u_s)` adding `Σ ‖u(k+j) − u_s‖²_S` over the decided steps to the incremental and
  the accumulated forms, where `R` weighs only the moves.  It settles the spare degree of freedom of a
  plant with more inputs than setpoints (an input as low as possible, another left free).
- **The error of an ARX model on data it was not fitted on**: `arxPredictionError(model, nk, u, y, env)`
  gives each output's RMS and the residual series of the one-step prediction — the loss
  `arxOrderSelection` ranks by — and `arxSimulationError` the same for a free run, the model's own
  outputs fed back, the stricter test of a model used over a horizon.
- **Numeric recursive least squares**: `rlsUpdate` over `_MatrixValue`/`Double`, answering
  `Either[RlsFailure, …]` with the reason, bit for bit the expression form's numbers.
- **An uncertified derivative-free minimiser**: `minimizeDerivativeFree(f, x0, lo, hi[, integer], method, env)`
  over an `Array[Double] => Double`, by pattern search (integer coordinates supported) or Nelder–Mead,
  steps relative to each variable's scale, deterministic.  Its answer is an `UncertifiedMinimum`, never
  a certified `MinimizeResult`.  New budget: `Limit.MaxDerivativeFreeEvaluations`.
- `arx`, `arxMimo` and the `arx` prompt function fit long data much faster: the excitation test reads
  the singular values off the triangular factor of a QR instead of the tall regressor matrix
  (2000 samples, 16 coefficients: about 7 ms on the JVM, where the SVD took 0.13–0.8 s).
- `svd` of a tall matrix costs what its size says, and with it `rank`, `pinv`, `cond`, `nullspace`,
  `orth` and `lsqfit`'s identifiability test: completing `U` no longer builds an `m×m` table, so
  `svd` of a 1983×16 matrix takes about 6 ms on the JVM (it took 0.13–0.44 s), and a 20000×20 one
  answers where it ran out of memory.

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
