---
title: Developer Guide
nav_order: 11
---

<img src="logo_bw.svg" alt="" height="80" style="float:right;margin:0 0 8px 16px"/>

---

# Leonardo Developer Guide

This guide is a complete onboarding reference for developers new to the Leonardo codebase.
It covers architectural decisions, recurring programming patterns, algorithm references, code
conventions, and a step-by-step recipe for adding new features.

---

## Table of contents

 1. [Project overview](#project-overview)
 2. [Package layering](#package-layering)
 3. [Core abstractions](#core-abstractions)
 4. [Dual evaluation model](#dual-evaluation-model)
 5. [Generic traversal: children / rebuild](#generic-traversal-children--rebuild)
 6. [Marker traits](#marker-traits)
 7. [Environment and precision](#environment-and-precision)
 8. [Recurring design patterns](#recurring-design-patterns)
 9. [Algorithm modules](#algorithm-modules)
10. [Memoisation](#memoisation)
11. [The parser](#the-parser)
12. [The REPL and Session](#the-repl-and-session)
13. [Algorithm references](#algorithm-references)
14. [Code conventions](#code-conventions)
15. [How to add a new AST node](#how-to-add-a-new-ast-node)
16. [How to add a new integration rule](#how-to-add-a-new-integration-rule)
17. [How to add a new domain package](#how-to-add-a-new-domain-package)
18. [Test conventions](#test-conventions)
19. [Build system](#build-system)

---

## Project overview

Leonardo is a Scala 3 symbolic math library.  It parses a string into an abstract syntax
tree (AST), then either evaluates the tree numerically (when all variables are bound) or
returns a simplified symbolic expression.  The library covers scalar algebra, matrix algebra,
equation solving, integral transforms, and first-order ODEs.

Read the [architecture page](architecture.md) for the high-level layering diagram and the
[expressions page](expressions.md) for user-facing usage examples.

---

## Package layering

Dependencies point **inward** toward `core`; no inner package imports an outer one.

```
core           _Expression, _Value, Environment, _Number, _Bool, _MatrixValue
  ↑
scalar         Sum, Product, Ratio, Power, functions, Derive, Integrate, Simplify, …
  ↑
matrix         _Matrix, MatSum, MatProduct, decompositions
  ↑
equation       _Equation, _Solve, SolveSystem
  ↑
transform      _Laplace, _Fourier, _InverseLaplace, LaplaceTransform
  ↑
ode            _ODE, SolveODE, SolveODESymbolic
  ↑
parser         Parser — string → AST; imports all domains
  ↑
cli            Repl, Session; leaf, nothing imports it
```

This graph is a strict DAG.  Adding a cross-layer import is a design violation and will
create a circular dependency in sbt.

---

## Core abstractions

Every node in the expression tree is a Scala `case class` (or `case object`) that extends
`_Expression`.  The naming convention is that AST-node files and types are prefixed with `_`
(`_Expression.scala`, `_Operation.scala`, …); unprefixed files are algorithm modules
(`Derive.scala`, `Simplify.scala`, …).

### `_Expression`

```scala
trait _Expression:
  def eval(env: Environment): Either[_Expression, _Value]
  def children: List[_Expression]
  def rebuild(newChildren: List[_Expression]): _Expression
  lazy val freeVars: Set[String] = children.flatMap(_.freeVars).toSet
```

`_Expression` is the root.  Every node must implement all three methods.

### `_Value`

```scala
trait _Value extends _Expression
```

Concrete, fully-reduced results: `_Number(d)`, `_Complex(re, im)`, `_MatrixValue(…)`,
`_Bool(b)`.  A free variable like `_Variable("x")` is **not** a `_Value`; it is a symbolic
atom.

---

## Dual evaluation model

`eval` has two outcomes:

| Return | Meaning |
|---|---|
| `Right(v: _Value)` | All variables were bound; the node collapsed to a concrete value |
| `Left(e: _Expression)` | Some variable is still free; `e` is the most-reduced symbolic form |

Implementation skeleton:

```scala
override def eval(env: Environment): Either[_Expression, _Value] =
  val ra = a.eval(env)
  val rb = b.eval(env)
  (ra, rb) match
    case (Right(_Number(x)), Right(_Number(y))) => Right(_Number(x + y))  // numeric path
    case _                                      => Left(Sum(ra.toExpression, rb.toExpression))
```

The `toExpression` extension (defined in `core._Expression`) collapses `Either[_Expression, _Value]`
back to `_Expression` when rebuilding symbolic nodes from partially-reduced operands.

**Fixpoint convention.** Functional nodes (`_Integral`, `_Derivative`, `_Limit`, `_Laplace`,
`_ODE`, …) stay symbolic by returning `Left(this)` when no rule fires.  They check for the
fixpoint *before* recursing, so returning the same node guarantees termination.

---

## Generic traversal: children / rebuild

`children` lists the sub-expressions that algorithms recurse into.  `rebuild` reconstructs
the same node shape with replacement children.  Together they enable generic traversal
without matching every case in every algorithm:

```scala
// Substitute every variable in any node type:
case class Substitute(definitions: Map[String, _Expression]) extends (_Expression => _Expression):
  def apply(e: _Expression): _Expression =
    val substituted = e.rebuild(e.children.map(this))
    substituted match
      case _Variable(name) => definitions.getOrElse(name, substituted)
      case other           => other
```

**Binder variables** (the differentiation variable in `_Derivative`, the integration variable
in `_Integral`, etc.) are intentionally **excluded** from `children` so `Substitute` and
`Analysis.dependsOn` do not recurse into them.  `rebuild` carries binder positions through
unchanged, as the extra first argument by convention.

---

## Marker traits

### `_ElementWise`

Tagging a node with `_ElementWise` tells algorithm passes (derive, simplify, expand,
integrate) that they may legally distribute over `children` and `rebuild` the same shape.
Only mathematically linear containers qualify — `_Equation` is linear (`d/dx (lhs = rhs)` =
`d/dx(lhs) = d/dx(rhs)`); `Product` is not (it requires the product rule).

### `_MatrixShaped`

A subset of `_ElementWise` for symbolic matrix literals.  Exposes `rows` and `cols` in
addition to `children`/`rebuild`.  This lets `scalar._Function` distribute `exp(A)`, `sin(A)`,
… over a symbolic matrix argument without importing the `matrix` package.

### Design rule

Marker traits live in `core` so all domain packages can opt in without circular imports.

---

## Environment and precision

`Environment` is an immutable map from variable names to `_Value`s plus a `precision` field.

```scala
val env = new Environment()                       // empty, DefaultPrecision = 5
val env2 = env.withBinding("x", _Number(3.14))   // scoped copy — original unchanged
```

`eval` always accepts an `Environment`; it does not modify global state.  Passing
`withBinding` into recursive `eval` calls is how binder variables are scoped during
`_DefIntegral`, `_ODE`, etc.

`Environment` carries four fields today — `precision`, the variable bindings,
`symmetricLogic` and `semantics` — the last two added as *defaulted* parameters so
existing call sites keep compiling (see [Recurring design patterns](#recurring-design-patterns),
pattern 4).

`Environment.DefaultPrecision = 5` is the single source of truth for rounding, and one
point is easy to get wrong: **rounding is a display concern only**.  `_Number.eval` is
`Right(this)` and never rounds — the only `_Number.round` call sites are in `toString`
and `display(p)`, and the same holds for `_Complex` and `_Truth`.  A computation
therefore carries full `Double` precision from end to end; `precision` decides only how a
result is *shown*.

The single place `precision` changes **semantics** is the equality tolerance in
`equation.compareSides` (`0.5 · 10⁻ᵖ`), which is what makes `sin(pi) = 0` evaluate to
`true` despite the floating-point residue.

A related trap: `_Number.toString` is fixed at `DefaultPrecision` and ignores the session
setting, so anything rendered through it — rather than through `Session.formatExpression`
or `truthCell` — will print at 5 decimals however the session is configured.

---

## Recurring design patterns

These eight patterns recur across the domains.  Recognising them is usually enough to
predict how a new feature should be built, and most review comments on this codebase come
down to one of them.

### 1. Sibling value types with a collapsing factory

When a richer numeric domain is added, it becomes a **sibling** of the simpler one rather
than replacing it, and its companion carries a smart `of` factory that **collapses back to
the simpler type whenever nothing is lost**:

```scala
_Complex.of(3.0, 0.0)   // _Number(3.0)   — a zero imaginary part is just a real
_Truth.of(1.0)          // _Bool(true)    — a crisp degree is just a boolean
_Truth.of(0.3)          // _Truth(0.3)    — genuinely graded, so it stays
```

The payoff is that **every `case _Number(x)` and `case _Bool(b)` already written keeps
firing**.  Adding complex numbers did not invalidate the real fast path, and adding fuzzy
degrees did not invalidate the boolean one.  The constructors are `private`, so `of` is the
only route in and the invariant cannot be broken from outside.

Do this rather than widening the existing type in place — the latter is a rewrite of every
pattern match in the library.  Worked examples: `core/_Complex.scala` and `core/_Truth.scala`.

### 2. Widening readers

The dual of pattern 1.  Where a factory narrows on the way *out*, a reader **widens on the
way in**, so one rule table serves every value in the tower:

```scala
asTruth(_Bool(true))   // Some(1.0)
asTruth(_Truth(0.3))   // Some(0.3)
asTruth(_Number(2))    // None  -> the node stays symbolic
```

This is the `Int` → `Double` analogy: you do not write `+` twice, you widen the `Int`.
Because of it the boolean, Kleene and fuzzy logics share *literally* one implementation of
`And` — the boolean truth table is not a special case in the code, it is a special case of
the arithmetic.

### 3. Domain errors stay symbolic — the "Asin convention"

A function that cannot produce a correct concrete answer returns `Left` (stays symbolic)
rather than guessing, throwing, or propagating a non-finite value:

| Situation | Result |
|---|---|
| `ln(0)`, `x/0` | stays symbolic — not `-Infinity`, not `NaN` |
| `fact(171)` | stays symbolic — `171!` overflows a `Double` |
| `Gamma(0)` | stays symbolic — a pole |
| `asin` of a complex argument | stays symbolic — outside the implemented domain |
| `trimf` with out-of-order feet | stays symbolic — not a curve |

Numeric kernels signal this by returning `Option[Double]`; the node maps `None` to
`Left(this)`.  A symbolic result is a *useful* answer — the user can see what did not
reduce — whereas an infinity silently poisons everything downstream.

### 4. Defaulted `Environment` fields for behaviour toggles

New evaluation modes are added as **defaulted** fields on `Environment`, never as new node
types or forked packages:

```scala
class Environment(precision:      Int             = DefaultPrecision,
                  variables:      Map[String, _Value] = Map(),
                  symmetricLogic: Boolean         = false,           // 4.G
                  semantics:      LogicSemantics  = MinMax)          // 4.H
```

Defaulting keeps roughly 500 existing positional call sites compiling untouched, and the
default path stays byte-identical, which makes "the whole existing suite still passes" a
meaningful check rather than a coincidence.  `withBinding` must thread **every** field
through, or a mode silently evaporates inside a scoped evaluation.

Put a knob here — not in `cli` — when a *bound variable* has to participate.  A
parse-time-only encoding would convert literals but not `a` after `a := 0`.

### 5. Injected passes to preserve layering

`logic` may not import `scalar`, yet `simplifyLogic` must simplify scalar sub-expressions
nested inside connectives.  Rather than weaken the layering, the pass is **injected**:

```scala
simplifyLogic(e, simplifyLeaf = scalar.simplifyFully)   // the REPL supplies it
simplifyLogic(e)                                        // default: identity
```

The caller that already depends on both packages (`cli`) supplies the missing capability.
Reach for this whenever a lower layer needs a higher one's behaviour.

### 6. Give-up guards and hard caps

Every algorithm that can blow up carries an explicit bound, and exceeding it returns the
input **unchanged** rather than a wrong or enormous answer:

| Guard | Where | Bound |
|---|---|---|
| `MaxTaylorOrder` | `Series.scala` | 20 |
| power expansion | `Expand.scala` | 20 |
| `MaxNormalFormClauses` | `NormalForm.scala` | 1024 (checked *before* materialising) |
| `MaxTruthTableVars` / `MaxKleeneTableVars` | `TruthTable.scala` | 16 / 10 |
| `MaxSymbolicDim` | `_MatrixOperation.scala` | 6 |
| `MaxDepth` | `Parser.scala` | 500 |

Alongside the size caps sit **content** guards, which abort when a sub-result is not good
enough to build on: `hasDerivative` in `Series.scala` refuses to emit a Taylor polynomial
whose coefficients still contain an unevaluated `_Derivative`, exactly as `hasIntegral` in
`SolveODESymbolic.scala` refuses a solution containing an unevaluated `_Integral`.  A
partial answer that *looks* complete is worse than an honest symbolic one.

### 7. Binders versus free variables

Most `_Functional` nodes take a variable that is a **binder** — it is excluded from
`children` so `substitute` cannot rewrite it, and it does not appear in the result:
`derive(e, x)`, `integral(e, x)`, `defuzz(e, v, lo, hi)`.

`_Taylor` is the deliberate exception, and it is worth understanding before adding another
series node.  Its variable is the *expansion* variable: it appears **free in the result**,
which is a polynomial in `(v − point)` — the role `_Laplace`'s output variable plays.  It
is still kept out of `children` and carried through `rebuild`, because `substitute` must
not rewrite the variable an expansion is taken in.  So the two questions are independent:

- *in `children`?* — can `substitute` and friends rewrite it?
- *free in the result?* — does it survive into the output?

### 8. Protecting the variable namespace

Grammar keywords are reserved, which permanently removes them as variable names.  For
`sin` or `defuzz` that is free; for a name mathematicians actually bind it is not.

`gamma` and `beta` are among the most common variable names in physics and statistics, and
`ParserTest` documents `alpha + beta` parsing as two variables.  So the functions are
spelled **`Gamma(z)` and `Beta(x, y)`** — capitalised, and reserved in that form only.  The
grammar is case-sensitive, so the lowercase names stay available.  `lgamma` needs no such
treatment: nobody binds it.

Weigh this whenever a new keyword is added.  Names that merely *start* with a reserved word
(`gamma1`, `betaX`, `sina`) are always still legal.

---

## Algorithm modules

All algorithm modules in `scalar/` are package-level functions (Scala 3 top-level `def`).
They do not form a class or object hierarchy — they are imported where needed:

| File | Entry points |
|---|---|
| `Derive.scala` | `derive(e, v)`, `deriveN(e, v, n)`, `derive(e, v1, v2, ...)` |
| `Integrate.scala` | `integrate(e, v)` |
| `Simplify.scala` | `simplify(e)`, `simplifyFully(e)` |
| `Expand.scala` | `expand(e)` |
| `Normalize.scala` | `collect(e, v)`, `normalize(e, v)` |
| `Analysis.scala` | `dependsOn(e, v)` |
| `Substitute.scala` | `substitute(e, definitions)` |
| `Compile.scala` | `compile(e, v, env)` |
| `Sample.scala` | `sample(e, v, lo, hi, n, env)` |
| `Limit.scala` | `evalLimit(e, v, point, dir, env)` |

`Syntax.scala` wraps all of these as extension methods on `_Expression` so callers can write
`e.derive(v)` instead of `derive(e, v)`.  Import with `import scalar.Syntax.*`.

---

## Memoisation

`derive` and `simplify` are memoised via `Memo[K, V]` — a bounded `ConcurrentHashMap`
with clear-on-overflow eviction.

```scala
private val deriveMemo = new Memo[(_Expression, String), _Expression](10000)
```

Memoisation is semantically transparent because:
- Both functions are **pure** in their arguments.
- `_Expression` nodes are immutable `case class` instances — structural equality and
  hashCode are correct by default.
- Results never depend on `Environment` (algorithms do not take one).

The cache is `private[scalar]`, so it is invisible to callers outside the package.

---

## The parser

`parser/Parser.scala` extends `JavaTokenParsers` (scala-parser-combinators).  It uses
recursive descent with explicit priority levels:

```
topLevel → expr ("=" | "==") expr
expr     → ["+" | "-"] simpleExpr
simpleExpr → term (("+"|"-") term)*
term     → signedPower (("*"|"/") signedPower | implicit signedPower)*
power    → factor ["^" signedPower]
factor   → function | functional | matrix | value | "(" expr ")"
```

Key parser decisions:
- **Right-associative `^`**: `power` recurses into itself on the right to get `2^3^2 = 512`.
- **Implicit multiplication**: `3sin(x)` and `3x` are both legal; the implicit operand is
  unsigned (`3-2` is subtraction, not `3 * (-2)`).
- **Word-boundary guards**: `pi`, `e`, `i` are always constants; reserved names like
  `sin`, `derive`, `solve` cannot be variable names.
- **Matrix dispatch**: when one operand is structurally a `_Matrix` or `_MatrixOperation`,
  `+`/`-`/`*` build `MatSum`/`MatProduct`/`MatScale` instead of scalar nodes.  This is
  purely structural at parse time; runtime dispatch handles the remaining ambiguity.

Adding a new function to the parser: add a `def newFun` combinator in the `function`
alternative of `factor`, then wire it into `function` with `|`.

---

## The REPL and Session

`cli/Repl.scala` is the interactive shell.  The session logic lives entirely in
`Session.execute(line: String): String` — a pure function with no IO — so it is fully
testable without starting the REPL.

Session state is stored in two mutable maps inside `Session`:
- `env: Environment` — `_Value` bindings (numbers, matrices, booleans)
- `definitions: Map[String, _Expression]` — symbolic expressions, late-bound

Late-bound definitions are substituted via `scalar.Substitute` at use time, so redefining
a dependency retroactively changes all late-bound expressions that mention it.
`consolidate(expr)` is the explicit freeze operation: it substitutes current definitions,
simplifies, and stores the result as a frozen expression body that no later redefinition
can change.

`Session.resolveMatrixOps` re-types scalar `Sum`/`Product`/`Ratio` over matrix-shaped
operands into `MatSum`/`MatProduct`/`MatScale` before simplify/expand.  This is necessary
because the parser cannot always determine at parse time whether a variable will be bound
to a matrix.

---

## Algorithm references

### Symbolic differentiation

Standard chain rule, product rule, quotient rule, and known derivatives.  The rule table is
in `Derive.scala`; memoised per `(expression, variable-name)` pair.

### Indefinite integration

Multi-tier rule table in `Integrate.scala`:

| Tier | Technique | Reference |
|---|---|---|
| Linear chain rule | `∫f(a·v+b)dv = F(a·v+b)/a` | Standard substitution |
| Integration by parts | `∫u dv = u·V − ∫V du`, LIATE heuristic | [Wikipedia — Integration by parts](https://en.wikipedia.org/wiki/Integration_by_parts) |
| Trig-power reduction | `∫sinⁿ`, `∫cosⁿ` reduction formula | [Wikipedia — Reduction formula](https://en.wikipedia.org/wiki/Reduction_formula) |
| Rational functions | Long division → partial fractions → completing the square / residues | [Wikipedia — Partial fractions in integration](https://en.wikipedia.org/wiki/Partial_fraction_decomposition#Application_to_symbolic_integration) |

The rational-function tier uses `polyRoots` (companion-matrix eigendecomposition) to find
the denominator roots for the residue method:
[Wikipedia — Companion matrix](https://en.wikipedia.org/wiki/Companion_matrix)

### Definite integration (Simpson's rule)

`_DefIntegral.eval` uses composite Simpson's rule:
[Wikipedia — Simpson's rule](https://en.wikipedia.org/wiki/Simpson%27s_rule)

The fast path compiles the integrand to a `Double => Double` JVM closure via
`Compile.compile` (one JVM allocation per evaluation, not one tree walk).

### Limit computation (`Limit.scala`)

Four-tier engine, most specific first:

1. **Direct substitution** — bind `v = point` and evaluate.
2. **L'Hôpital's rule** — for 0/0 or ∞/∞ forms, differentiate numerator and denominator
   (up to 5 times).  [Wikipedia — L'Hôpital's rule](https://en.wikipedia.org/wiki/L%27H%C3%B4pital%27s_rule)
3. **Structural rules at ±∞** — polynomial degree comparison, `exp`/`ln`/`atan` shapes.
4. **Epsilon perturbation** — sample at `point ± ε` and `point ± 0.001ε`; agree within a
   relative tolerance of 1e-5.  For two-sided limits both flanks must agree with each other
   as well.

### Simplification

Single-pass bottom-up structural rewriting (`simplify`); fixpoint iteration (`simplifyFully`).
No Knuth–Bendix completion — rules are hard-coded and termination is guaranteed by the
fixed rule set.

### Expression compilation (`Compile.scala`)

Traverses the AST once and emits a Scala closure `Double => Double`.  Returns `None` for
any node shape it cannot compile.  Used as the O(1)-per-sample fast path inside
Simpson's rule and `sample`.

### Normalization (`Normalize.scala`)

`collect(e, v)` extracts dense polynomial coefficients in `v` using:
- Addition → zip-add coefficient vectors
- Multiplication → convolution (polynomial multiplication)
- Constant denominators → divide-through coefficients
- Integer literal powers ≤ 20 → expand via convolution

`normalize(e, v)` rebuilds from the folded coefficients, combining like terms.

### Equation solving (`Solve.scala`)

Tiers, most specific first:

| Shape | Method |
|---|---|
| Matrix unknown — structured forms (`A·X=B`, etc.) | Inverse kernel or cofactor expansion (symbolic ≤ 6×6) |
| Matrix unknown — general linear (`A·X + X·B = C`) | Kronecker vectorization:  `vec(L·X·R) = (Rᵀ ⊗ L)·vec(X)` |
| Scalar linear | `collect` → `v = −c₀/c₁` |
| Scalar quadratic | Discriminant |
| Scalar transcendental / degree ≥ 3 | Sign-change scan + bisection |

Kronecker vectorization reference:
[Wikipedia — Vectorization (mathematics)](https://en.wikipedia.org/wiki/Vectorization_(mathematics))

Linear system solver uses Gaussian elimination with partial pivoting.

### Matrix decompositions (`_MatrixValue.scala`)

| Decomposition | Algorithm | Reference |
|---|---|---|
| LU | Partial pivoting (Doolittle) | [Wikipedia — LU decomposition](https://en.wikipedia.org/wiki/LU_decomposition) |
| QR | Modified Gram–Schmidt | [Wikipedia — QR decomposition](https://en.wikipedia.org/wiki/QR_decomposition) |
| Eigenvalues | QR iteration with Wilkinson shifts; 2×2 analytic fallback for complex pairs | [Wikipedia — QR algorithm](https://en.wikipedia.org/wiki/QR_algorithm) |
| Eigenvectors | Null-space RREF for each eigenvalue | [Wikipedia — Eigendecomposition](https://en.wikipedia.org/wiki/Eigendecomposition_of_a_matrix) |
| Jordan | Spectral decomposition (diagonalizable case); stays symbolic for defective matrices | [Wikipedia — Jordan normal form](https://en.wikipedia.org/wiki/Jordan_normal_form) |

Dense matrix multiply uses 64×64 block tiling (L1-cache-sized tiles) and parallel row
blocks above a 2¹⁶ work threshold.

### Laplace transform (`LaplaceTransform.scala`)

Rule table over standard forms.  Key rules:

| Rule | Expression | Result |
|---|---|---|
| Linearity | `∫ (a·f + b·g) e^{−st} dt` | `a·L{f} + b·L{g}` |
| Power | `L{tⁿ}` | `n! / sⁿ⁺¹` |
| Exponential | `L{e^{at}}` | `1 / (s−a)` |
| First shift | `L{e^{at} g(t)}` | `G(s−a)` |
| Second shift | `L{u(t−a) g(t−a)}` | `e^{−as} G(s)` |
| Derivative of transform | `L{tⁿ g(t)}` | `(−1)ⁿ dⁿG/dsⁿ` |

Reference: [Wikipedia — Laplace transform](https://en.wikipedia.org/wiki/Laplace_transform)

Fourier transform is computed as `L{e(t)}|_{s=iω}` (Laplace-to-Fourier substitution).

### Ordinary differential equations (`SolveODE.scala`, `SolveODESymbolic.scala`)

Closed-form tier recognises the linear shape `y' = a(t)·y + b(t)`:
- **Constant coefficients**: exact `τ = target − t₀` forms.
- **Variable coefficients**: integrating-factor method `μ = e^{−∫a dt}`.
  Reference: [Wikipedia — Integrating factor](https://en.wikipedia.org/wiki/Integrating_factor)

Numeric fallback: classic 4th-order Runge–Kutta.
Reference: [Wikipedia — Runge–Kutta methods](https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods)

The step count scales with interval length so accuracy is maintained across arbitrarily
long integration spans.

---

### Logic: boolean, three-valued, and fuzzy (`logic/`)

One node hierarchy and **one rule table** cover all four logics; the *values* select which
logic you are in.  That is the whole design, and it is why `logic` has no per-tier
branching:

| Values | Logic | Carrier |
|---|---|---|
| `{0, 1}` | boolean | `core._Bool` |
| `{0, ½, 1}` | three-valued (Kleene) | `+ core._Truth.Unknown` |
| `{-1, 0, 1}` | symmetric ternary — an *encoding* of the above | same, different spelling |
| `[0, 1]` | fuzzy | `core._Truth` |

| Topic | Reference |
|---|---|
| De Morgan's laws (used by `nnf`) | [Wikipedia — De Morgan's laws](https://en.wikipedia.org/wiki/De_Morgan%27s_laws) |
| Negation normal form | [Wikipedia — Negation normal form](https://en.wikipedia.org/wiki/Negation_normal_form) |
| Conjunctive normal form | [Wikipedia — Conjunctive normal form](https://en.wikipedia.org/wiki/Conjunctive_normal_form) |
| Disjunctive normal form | [Wikipedia — Disjunctive normal form](https://en.wikipedia.org/wiki/Disjunctive_normal_form) |
| Kleene's strong three-valued logic | [Wikipedia — Three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic#Kleene_and_Priest_logics) |
| Balanced (symmetric) ternary | [Wikipedia — Balanced ternary](https://en.wikipedia.org/wiki/Balanced_ternary) |
| Fuzzy sets and membership functions | [Wikipedia — Fuzzy set](https://en.wikipedia.org/wiki/Fuzzy_set) · [Membership function](https://en.wikipedia.org/wiki/Membership_function_(mathematics)) |
| t-norms and t-conorms | [Wikipedia — t-norm](https://en.wikipedia.org/wiki/T-norm) · [t-conorm](https://en.wikipedia.org/wiki/T-norm#T-conorms) |
| Łukasiewicz logic | [Wikipedia — Łukasiewicz logic](https://en.wikipedia.org/wiki/%C5%81ukasiewicz_logic) |
| Defuzzification (centroid, mean-of-maxima, bisector) | [Wikipedia — Defuzzification](https://en.wikipedia.org/wiki/Defuzzification) |

**The three t-norm families** are selected by `Environment.semantics` and share the strong
negation `1 − a`:

| Semantics | `and` (t-norm) | `or` (t-conorm) |
|---|---|---|
| `MinMax` (Zadeh, default) | `min(a, b)` | `max(a, b)` |
| `Product` | `a · b` | `a + b − a·b` |
| `Lukasiewicz` | `max(0, a + b − 1)` | `min(1, a + b)` |

All three agree with classical logic on `{0, 1}`, which is why the boolean and ternary
tiers are unaffected by the choice.  Each has `0` as t-norm annihilator and `1` as t-conorm
annihilator, which is exactly what makes the `false and X` / `true or X` short-circuits
sound under every one of them.

**Two gates protect simplification**, and the distinction matters:

- *Classical-only* rules — complement (`a and not a = false`), `a implies a`, `a xor a` —
  fail already at `unknown`, so they are gated on `isCrisp`.
- *Lattice-only* rules — idempotence and absorption — hold for graded values under
  min–max but **not** under product or Łukasiewicz, where `a and a` is `a²`.  They carry
  the additional `semantics == MinMax` gate.

Everything else (constant folding, identity and annihilator elements, double negation) is
valid under every t-norm and fires unconditionally.  `xor` is defined *through* the same
`(a and not b) or (not a and b)` desugaring that `toCNF`/`toDNF` apply, so eval and the
normal forms cannot disagree — the naive `|a − b|` would answer `false` at
`unknown xor unknown`, contradicting both.

### Special functions (`SpecialFunctions.scala`)

| Function | Method | Reference |
|---|---|---|
| `fact(n)` | exact table for integers `0..170`; `Γ(n+1)` beyond | [Wikipedia — Factorial](https://en.wikipedia.org/wiki/Factorial) |
| `dfact(n)`, `mfact(n, k)` | step-`k` product | [Wikipedia — Double factorial](https://en.wikipedia.org/wiki/Double_factorial) |
| `Gamma(z)` | Lanczos approximation, `g = 7`, `n = 9` | [Wikipedia — Gamma function](https://en.wikipedia.org/wiki/Gamma_function) · [Lanczos approximation](https://en.wikipedia.org/wiki/Lanczos_approximation) |
| — negative arguments | reflection formula `Γ(z)Γ(1−z) = π/sin(πz)` | [Wikipedia — Reflection formula](https://en.wikipedia.org/wiki/Reflection_formula) |
| `lgamma(z)` | the same series evaluated **in log space** | [Wikipedia — Log-gamma](https://en.wikipedia.org/wiki/Gamma_function#Log-gamma_function) |
| `Beta(a, b)` | `exp(lgamma a + lgamma b − lgamma (a+b))` | [Wikipedia — Beta function](https://en.wikipedia.org/wiki/Beta_function) |

Two decisions here are worth copying elsewhere.  **The exact integer path is kept** rather
than routing everything through `Γ(n+1)`, so small factorials stay bit-accurate instead of
inheriting Lanczos error.  And **`lgamma` is computed in log space, not as `log(gamma z)`**
— that is the entire point of having it: `Γ(10⁵)` overflows a `Double` while `ln Γ(10⁵)` is
an ordinary number.  Working in logarithms is the standard remedy when magnitudes, not
precision, are the problem.

### Series expansions (`Series.scala`)

| Topic | Reference |
|---|---|
| Taylor series | [Wikipedia — Taylor series](https://en.wikipedia.org/wiki/Taylor_series) |
| Maclaurin series (centre 0) | [Wikipedia — Maclaurin series of common functions](https://en.wikipedia.org/wiki/Taylor_series#List_of_Maclaurin_series_of_some_common_functions) |
| Truncation error / Lagrange remainder | [Wikipedia — Taylor's theorem](https://en.wikipedia.org/wiki/Taylor%27s_theorem#Explicit_formulas_for_the_remainder) |
| Fourier series | [Wikipedia — Fourier series](https://en.wikipedia.org/wiki/Fourier_series) |
| Gibbs phenomenon (why a truncated series overshoots at a jump) | [Wikipedia — Gibbs phenomenon](https://en.wikipedia.org/wiki/Gibbs_phenomenon) |
| Pade approximant | [Wikipedia — Padé approximant](https://en.wikipedia.org/wiki/Pad%C3%A9_approximant) |

`taylorSeries` folds

```
Σ(k = 0 .. n)  f⁽ᵏ⁾(point) / k! · (v − point)ᵏ
```

over the **memoised** `deriveN`, so the k-th derivative reuses the (k−1)-th; each
coefficient is instantiated at the centre with `substitute`, and `simplifyFully` collapses
the `k = 0` term's `(v − point)⁰`.  The centre need not be numeric — expanding about a
symbolic `a` gives a genuine polynomial in `(v − a)`.

`fourierSeries` is the **numeric** counterpart, and the contrast is instructive.  Taylor
coefficients come from differentiation, which is symbolic and exact; Fourier coefficients
come from *integration over a period*, which here means Simpson's rule — so the period must
be a concrete number, an Environment is required, and the coefficients carry quadrature
error.  Two consequences a newcomer should expect rather than treat as bugs:

- **Terms that vanish analytically come back tiny, not zero.**  Every sine coefficient of
  an even function is   mathematically and about 1e-17 numerically.  They are *not*
  chopped: picking a threshold would silently discard genuinely small coefficients, and
  this library treats cleanup as a display concern.
- **Convergence is in the mean, not pointwise.**  A truncated Fourier series of a function
  with a jump overshoots near the discontinuity, and refining the order does not remove the
  overshoot — that is the Gibbs phenomenon, not a defect.  Test such a series against its
  own analytic partial sum, not against the function it expands.

### Numerical stability

Floating-point pitfalls the library has already been bitten by, worth recognising before
adding a numeric routine:

| Pitfall | Reference |
|---|---|
| Loss of significance (catastrophic cancellation) | [Wikipedia — Loss of significance](https://en.wikipedia.org/wiki/Catastrophic_cancellation) |
| The stable quadratic formula | [Wikipedia — Avoiding loss of significance](https://en.wikipedia.org/wiki/Quadratic_equation#Avoiding_loss_of_significance) |
| Vieta's formulas (the root product used to recover the small root) | [Wikipedia — Vieta's formulas](https://en.wikipedia.org/wiki/Vieta%27s_formulas) |
| IEEE 754 and machine epsilon | [Wikipedia — IEEE 754](https://en.wikipedia.org/wiki/IEEE_754) · [Machine epsilon](https://en.wikipedia.org/wiki/Machine_epsilon) |

**A worked example from this codebase.**  `solve(x^2 + 1e8*x + 1 = 0, x)` used to return
the small root 25% wrong.  With `b² ≫ 4ac`, `√Δ` equals `b` to within a few ulps, so
whichever of the `(-b ± √Δ)/2a` branches *subtracts* them keeps almost no significant
digits.  `quadraticRoots` now forms the larger-magnitude root first — where the terms share
a sign and therefore **add** — and recovers the other from the root product `x₁x₂ = c/a`:

```
q  = -(b + sign(b)·√Δ) / 2
x₁ = q / a
x₂ = c / q
```

The general lesson: when two nearly-equal quantities must be subtracted, look for an
algebraically equivalent form that adds instead.  Worth being precise about what an exact
tier would and would not have done here: `√(10¹⁶ − 4)` is irrational, so no rational
representation stores it exactly — but a *lazily* evaluated algebraic number does recover
the small root, because it defers rounding to the very end instead of at every step.  A
probe of spire's `Algebraic` returns `-1.00000000000000010000000000000E-8` for exactly this
equation.  That is the distinction between tiers 1 and 2 of the exact-arithmetic plan
below, and the reason tier 2 exists at all.

### Exact arithmetic — the rational tier (`core/_Rational.scala`)

The counterpart of the section above.  Numerical stability is about rearranging a formula so
that `Double` survives it, one formula at a time; this tier is about not using `Double` in
the first place, for every path at once.

**The model is bounded-denominator rational arithmetic** — not "exact irrationals", which is
impossible by definition.  Every value carries a rational approximation whose error is
bounded by a *working precision*, irrationals met along the way are re-approximated to that
bound, and the arithmetic runs on numerator and denominator as `BigInt`s.  Precision stops
being purely a display setting and becomes a knob a user who distrusts a result can raise
until the answer stops moving.

| Concept | Reference |
|---|---|
| Arbitrary-precision arithmetic | [Wikipedia](https://en.wikipedia.org/wiki/Arbitrary-precision_arithmetic) |
| Continued fractions (the approximation algorithm) | [Wikipedia](https://en.wikipedia.org/wiki/Continued_fraction) · [Simple continued fraction](https://en.wikipedia.org/wiki/Simple_continued_fraction) |
| Stern–Brocot tree (the same enumeration, seen as a tree) | [Wikipedia](https://en.wikipedia.org/wiki/Stern%E2%80%93Brocot_tree) |
| Diophantine approximation (what "best under a bound" means) | [Wikipedia](https://en.wikipedia.org/wiki/Diophantine_approximation) |
| Euclidean algorithm (the `gcd` whose cost drives the policy below) | [Wikipedia](https://en.wikipedia.org/wiki/Euclidean_algorithm) |
| Hilbert matrix (the benchmark's worst case) | [Wikipedia](https://en.wikipedia.org/wiki/Hilbert_matrix) |
| Gaussian elimination (where denominators blow up) | [Wikipedia](https://en.wikipedia.org/wiki/Gaussian_elimination) |

**Status.**  Slice A of the tier ships: `_Rational` is a sibling `_Value` of `_Number`, exact
for `+ - * /` and integer powers, with an `exact on | off` mode in the REPL.  Exact matrix
entries and an unbounded `fact` are slice B; genuinely high-precision transcendentals are
tier 2.  Off by default, so the `Double` path is unchanged.

#### The decision that made it affordable: a widening `_Number`

Adding a new `_Value` has a cost nobody budgets for.  A `_Rational` matches none of the
~108 `case _Number(x)` sites across the library, so on the first end-to-end run **eleven
features silently stopped working in exact mode** — matrix literals no longer collapsed,
`integral`, `limit`, `step`, the fuzzy membership curves and `derive`'s own power rule all
fell through to their symbolic fallbacks.  Nothing crashed; the answers just quietly got
worse, which is the failure mode worth fearing.

The fix is one extractor.  `core._Number.unapply` replaces the synthesized case-class one
and matches a `_Rational` as well, reading it as a `Double`:

```scala
def unapply(e: _Expression): Option[Double] = e match
  case n: _Number   => Some(n.d)
  case r: _Rational => Some(r.toDouble)
  case _            => None
```

which establishes the rule:

> `case _Number(x)` means **"reads as the real number `x`"**.  Code that must *preserve*
> exactness matches `case r: _Rational` explicitly — **and must place that case first**.

So float contagion is the default and exactness is opt-in.  That is the safe direction: a
missed opt-in degrades a result to the `Double` behaviour it already had, never produces a
wrong one.  And it cannot disturb existing code at all — outside exact mode no `_Rational` is
ever constructed, so the extra arm is unreachable.

The cost is that ordering became load-bearing in the four arithmetic operations and the
fourteen functions, which is exactly the kind of invariant that rots.  `ExactModeTest` pins
it from the outside: *turning exact mode on must not make any expression less reducible, and
must not change any number it produces.*  Both bugs above were found by that test, not by
reading the code.


**Why the representation is an in-house `BigInt` pair.**  spire is the intended engine for
*irrationals* (`Real` / `Algebraic`), but its `Rational` normalises to lowest terms on every
construction.  Adopting it as the representation would have settled the reduction question
below by fiat, before it could be measured.

#### The reduction policy, and why it was measured rather than chosen

`gcd` is the expensive step of rational arithmetic; skipping it makes each operation cheaper
but roughly *doubles* operand size.  Which wins is not obvious, so `GcdPolicy` offers three
answers — `Eager` (reduce always, what spire does), `Lazy` (never), `Threshold(bits)` (only
once an operand outgrows a bound) — and issue 4.M benchmarked them, with the exit criteria
written down *before* the numbers existed.  Run it with `sbt bench`; the full record is in
`docs/benchmarks/gcd-policy-2026-08.txt`.

The hypothesis under test was **not** "which is faster".  It was: *re-approximation to the
working threshold already caps operand size, so `Eager`'s advantage shrinks as
re-approximation runs more often.*  That is why every workload is swept across working
precisions rather than measured at one.

**The hypothesis held — and then its premise failed.**  At every finite working precision,
`Lazy` stayed within 2–3× of `Eager`'s operand size and was marginally *faster*.  But the
`exact` arm — no re-approximation at all — is not a synthetic control: it is the **normal**
mode for the operations that are closed over the rationals (`Sum`, `Product`, `Ratio`,
integer `Power`, and all of `matrix`), which never need to approximate anything.  There:

| 8×8 Hilbert solve, exact | max operand bits | ms | allocated |
|---|---|---|---|
| `Eager` | 30 | 0.16 | 0.10 MB |
| `Lazy` | **2 497 057** | **757** | **2 650 MB** |
| `Threshold(256)` | 256 | 0.06 | 0.15 MB |

An 83 000× operand-size ratio and a 4 700× slowdown, against an exit criterion that rejected
`Lazy` outright past 4×.  So: **`Threshold` wins, and it is not close.**  It keeps `Lazy`'s
cheapness wherever re-approximation is already bounding growth, and acts as a guard where
nothing else is — note that it beats `Eager` on that row too, because most operations never
reach the bound and skip the `gcd` entirely.

**The bound is coupled to the working precision, which is the subtle part.**  A bound *below*
the operand size a precision implies fires on every operation, making `Threshold` into
`Eager` under another name.  The sweep shows this directly in the deterministic `maxBits`
column: at 30 working digits a 64-bit bound reproduces `Eager`'s operand sizes exactly
(105 and 30 bits), while a 256-bit bound reproduces `Lazy`'s (186 and 75).  Since a
`d`-digit value needs `d·log₂10 ≈ 3.32d` bits and a product of two reaches twice that,
`_Rational.thresholdFor(digits)` uses `max(256, 8·digits)`.  Hard-coding a bound without
reference to the precision would silently give the benefit back.

**Read that benchmark honestly.**  Bit lengths are deterministic and repeat exactly across
runs; the sub-millisecond wall-clock figures carry JIT and scheduler noise of roughly a
factor of two, and the harness is a plain warm-up-and-take-the-minimum loop, not JMH.  The
effects the decision rests on are 100×–4 700× and sit far outside that noise; the 1.0–1.9×
ratios in the finite-precision rows do not, and should not be read as rankings.  This is why
the plan asked for operand size *alongside* time — it is the variable that explains the
wall-clock and predicts precisions that were never measured.

**All three policies are kept**, though only one is the default.  The plan said to delete the
losers, but a policy is one `match` arm each, and deleting them would delete the cross-policy
equality test — which asserts that all three produce *equal values* on every workload, and is
the only thing exercising the `Lazy` path at all.  That test outlives the decision; the
implementations it needs cost twenty lines.

## Code conventions

### `Option` not `null`

`null` must never appear in Scala code.  Wrap nullable Java APIs at the boundary:
`Option(javaMap.get(k))`, `Option(...).getOrElse(...)`.  The commit hooks fail on any
`null` usage in Scala files.

### `Option` accessors

- Never compare `Option` to `Some(x)` with `==`.  Use `opt.contains(x)`.
- Never call `.get` on an `Option`.  Use `fold`, `map`, `getOrElse`, or pattern matching.
- Never compare `opt == None`.  Use `opt.isEmpty`.

### No redundant conversions

`.toList` on a value already typed as `List` is flagged and fails the push hooks.  Check
the static type before adding any `.toXxx` call.

### Tail recursion

Long recursions over potentially large expression trees should be `@annotation.tailrec`.
Example: `flattenSum` in `Solve.scala` uses an explicit accumulator pattern.

### Comments

Write a comment only when the *why* is non-obvious — a hidden constraint, a mathematical
invariant, a workaround.  Do not describe what the code does (names already do that), and
do not reference the PR or issue number (that belongs in the commit message).

### Documentation

All public types and `def`s carry a `/** … */` ScalaDoc comment.  The first sentence is a
self-contained synopsis.  Tag every parameter, result, and type parameter.  See `CLAUDE.md`
for the full documentation convention.

---

## How to add a new AST node

Follow these steps using `_Heaviside` as a worked example.

### Step 1 — Define the case class

Place it in the appropriate package file.  Simple unary functions go in `_Function.scala`:

```scala
/** Unit step function.  Evaluates to 1 when `a ≥ 0`, 0 otherwise.
 *  Stays symbolic when `a` contains free variables.
 *
 *  @param a the argument expression
 */
case class _Heaviside(a: _Expression) extends _Expression:
  override def eval(env: Environment): Either[_Expression, _Value] =
    a.eval(env) match
      case Right(_Number(d)) => Right(_Number(if d >= 0.0 then 1.0 else 0.0))
      case result            => Left(_Heaviside(result.toExpression))

  override def children: List[_Expression] = List(a)
  override def rebuild(c: List[_Expression]): _Expression = _Heaviside(c.head)
  override def toString: String = s"step($a)"
```

### Step 2 — Add a parser rule

In `Parser.scala`, add a combinator inside the `function` parser:

```scala
def heaviside: Parser[_Expression] =
  "step(" ~> expr <~ ")" ^^ { e => _Heaviside(e) }
```

Wire it into `function`:

```scala
def function: Parser[_Expression] = heaviside | exp | ln | ...
```

### Step 3 — Handle it in algorithm modules

Each algorithm needs a case for the new node.  If not handled, the node is returned
unchanged (fixpoint = stays symbolic).

- `Simplify.scala` — add constant-folding / identity rules.
- `Derive.scala` — add the derivative rule (e.g. `_Heaviside` → `_DiracDelta` if that exists).
- `Integrate.scala` — add the antiderivative rule.
- `Compile.scala` — add a compilation rule if numeric sampling should work.

### Step 4 — Add tests

Add test cases to the most relevant test file.  For a new function the usual coverage is:
- Constant-folding (each branch of `eval`)
- Symbolic round-trip (`parse → toString → parse`)
- Derivative (if defined)
- Integral (if defined)
- Sampling (if compilable)

---

## How to add a new integration rule

`integrate(e, v)` in `Integrate.scala` is a large `match` over expression shapes.  To add
a rule:

1. Identify where in the match block to insert it (rules are tried top-to-bottom; more
   specific patterns must come before general ones).
2. Write the pattern:
   ```scala
   case MyNode(inner) if linearSlope(inner, v).isDefined =>
     val a = linearSlope(inner, v).get   // safe — just tested
     // build antiderivative and return it
   ```
3. If the new rule delegates to `integrate` recursively, ensure it makes progress (reduces
   the expression in some way) to avoid infinite recursion.
4. Add the rule to `IndefiniteIntegrationTest.scala`.

For rational-function shapes, the existing `integrateRational` helper handles the
polynomial long-division + partial-fractions pipeline; hook into it rather than duplicating
the logic.

---

## How to add a new domain package

Follow the `matrix` or `transform` package as a template.

1. Create `src/main/scala/<name>/` and `src/test/scala/<name>/`.
2. Add a `package.scala` with the package-level ScalaDoc.
3. Use the chained package declaration:
   ```scala
   package it.grypho.scala.leonardo
   package <name>
   ```
4. Import `core.*` and any other packages the new domain depends on (following the
   layering DAG — never import a package that is at the same level or above in the graph).
5. Register the new test suite in `build.sbt` if needed (currently discovered automatically).
6. Add the package to `parser/Parser.scala` so the grammar covers the new nodes.
7. Wire the new AST nodes into `Session.execute` in `cli/Repl.scala` if REPL display needs
   special handling.
8. Update `struct.puml`, `docs/src/architecture.md`, and `CLAUDE.md`.

---

## Test conventions

Tests use `AnyFlatSpec` + `BeforeAndAfter`.  Test files mirror the main source layout under
`src/test/scala/`.

```scala
package it.grypho.scala.leonardo
package scalar

import core.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter

class MyFeatureTest extends AnyFlatSpec with BeforeAndAfter:
  "feature" should "do something" in {
    val result = ...
    assert(result == expected)
  }
```

### Assert mathematical bounds, not magic tolerances

When a numeric result is approximate, assert against the **bound the mathematics
guarantees** rather than a hand-picked constant.  A tolerance someone chose says nothing
about why the residual is the size it is, and it fails for the wrong reason the moment an
order or a sample count changes.

```scala
// weak: 1e-6 is arbitrary, and this FAILS for a correct order-7 series at x = 1
assert(math.abs(series - math.sin(at)) < 1e-6)

// strong: sin is alternating with decreasing terms, so the error is bounded by the
// first omitted term. Self-explaining, and sharper.
val bound = math.pow(at, 9) / (1 to 9).product
assert(math.abs(series - math.sin(at)) <= bound + 1e-12)
```

Useful bounds: the **first omitted term** for an alternating series with decreasing
terms, the **Lagrange remainder** `e^a·a^(n+1)/(n+1)!` for `exp` on `[0, a]`, and
**Vieta's formulas** for polynomial roots.  Where no bound exists, prefer a *relative*
claim ("raising the order buys at least six decades") over an absolute one.

This is not hypothetical: three first-draft assertions in `SeriesTest` failed while the
implementation was correct — the residuals were exactly the truncation error.

Run a single suite:
```
sbt "testOnly it.grypho.scala.leonardo.scalar.MyFeatureTest"
```

Run a domain:
```
sbt "testOnly it.grypho.scala.leonardo.scalar.*"
```

Run all:
```
sbt test
```

For numeric results, use a tolerance:
```scala
assert(math.abs(result - expected) < 1e-4)
```

---

## Build system

The project uses SBT 1.10.11.  The version number is derived from git tags via
`sbt-dynver` — do not hardcode it.

Key build file sections:

| Section | Purpose |
|---|---|
| `scalacOptions` | `-explain`, `-deprecation`, `-feature`, `-Wconf:src=.*package\\.scala:silent` |
| `libraryDependencies` | `scala-parser-combinators` 2.4.0, ScalaTest 3.2.19 |
| `sbt site` | Runs `puml` + `mdoc` + `doc` + `injectApiStyles` for the full docs site |
| `sbt doc` | Scaladoc API → `target/scala-3.3.6/api` |

The `-Wconf:src=.*package\\.scala:silent` option suppresses the "No class, trait or object
defined" structural warning for pure package-doc stub files.

The public docs site is hosted on GitHub Pages (Just the Docs theme) and rebuilt
automatically by `.github/workflows/pages.yml` on every push to `main`.  Prose pages go in
`docs/src/` with Jekyll front matter (`title`, `nav_order`).  The `mdoc` plugin verifies
all ` ```scala mdoc ``` ` code blocks in the docs.
