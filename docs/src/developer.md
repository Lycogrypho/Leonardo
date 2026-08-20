---
title: Developer Guide
nav_order: 10
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
8. [Algorithm modules](#algorithm-modules)
9. [Memoisation](#memoisation)
10. [The parser](#the-parser)
11. [The REPL and Session](#the-repl-and-session)
12. [Algorithm references](#algorithm-references)
13. [Code conventions](#code-conventions)
14. [How to add a new AST node](#how-to-add-a-new-ast-node)
15. [How to add a new integration rule](#how-to-add-a-new-integration-rule)
16. [How to add a new domain package](#how-to-add-a-new-domain-package)
17. [Test conventions](#test-conventions)
18. [Build system](#build-system)

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

`Environment.DefaultPrecision = 5` is the single source of truth for rounding.

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
