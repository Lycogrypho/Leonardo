# Changelog

Release-level history: one entry per tag, or per group of tags where the individual ones were
development markers rather than releases.

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
