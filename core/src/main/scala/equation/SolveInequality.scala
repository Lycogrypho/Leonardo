package it.grypho.scala.leonardo
package equation

import core.*
import scalar.*
import logic.{And, Or}


/** Inequality solving — issue 3.2.
 *
 *  `solve(x^2 - 4 > 0, x)` answers `x < -2 or x > 2`.  The counterpart of [[solve]], which
 *  handles `=`; this handles `<`, `>`, `<=`, `>=` and `!=`.
 *
 *  **No new carrier was needed.**  A solution set is a *set*, and the obvious worry is that
 *  it wants an interval-valued `_Value`.  Since 4.R it does not: the answer is expressible in
 *  the language itself — a [[_Comparison]], an `or` of two, `and` for a bounded range, and
 *  `_Bool(true)` / `_Bool(false)` for the universal and empty sets.  This is the 4.P lesson
 *  applied again ("adding a `_Value` is cheap only when nothing has to read it as an existing
 *  one"), with the happier outcome that here one is avoided altogether.
 *
 *  **`equation` imports `logic` for this**, which is new but acyclic: `logic` imports only
 *  `core` and `scalar.compile`, never `equation`.  `probability.Predicate` already depends on
 *  both packages the same way.
 *
 *  **The direction flip is the whole problem.**  Dividing by a negative coefficient reverses
 *  the relation, so every tier asks [[scalar.sign]] first and refuses when the answer is
 *  `None`.  Refusing rather than assuming positivity is the same call 4.R slice B made when
 *  it declined `prob(2*X < 6)`: the bad outcome is not an error, it is a confident answer to
 *  a different question.
 */

/** Solves `c` for `v`, or `None` when no tier applies (the caller then stays symbolic).
 *
 *  @param c   the comparison to solve
 *  @param v   the variable to solve for
 *  @param env bindings and precision
 *  @return the solution set written in the language, or `None`
 */
private[equation] def solveInequality(c: _Comparison, v: _Variable,
                                      env: Environment): Option[_Expression] =
  // The -1 must join the comparison's own tier, or an exactly-stated inequality is demoted
  // through float contagion before the solver runs (the numeric-tier rule, issue 4.N).
  val minusOne   = _Rational.literalLike(-1, c)
  val difference = Sum(c.lhs, Product(minusOne, c.rhs))

  collect(difference, v) match
    case Some(cs) => polynomialTier(cs, c.op, v, env)
    // `collect` gives up on a denominator containing `v`, which is exactly the rational tier.
    case None     => rationalTier(difference, c.op, v, env)

/** Linear and quadratic tiers over `collect`'s coefficient vector. */
private def polynomialTier(cs: Vector[_Expression], op: CompareOp,
                           v: _Variable, env: Environment): Option[_Expression] =
  cs.size match
    case 1 => constantTier(cs(0), op, env)
    case 2 => linearTier(cs(0), cs(1), op, v, env)
    case 3 => quadraticTier(cs(0), cs(1), cs(2), op, v, env)
    case _ => None

/** `v` does not occur, so the relation is a constant truth: `2 < 3` is `true`. */
private def constantTier(c0: _Expression, op: CompareOp, env: Environment): Option[_Expression] =
  _Comparison(c0, op, _Rational.literalLike(0, c0)).eval(env) match
    case Right(b: _Bool) => Some(b)
    case _               => None

/** `c1*v + c0 <op> 0`  ->  `v <op'> -c0/c1`, with `op'` flipped when `c1` is negative. */
private def linearTier(c0: _Expression, c1: _Expression, op: CompareOp,
                       v: _Variable, env: Environment): Option[_Expression] =
  sign(c1, env) match
    case Some(s) if s != 0 =>
      val bound = simplifyFully(Ratio(Product(_Rational.literalLike(-1, c0), c0), c1))
      Some(_Comparison(v, if s < 0 then flip(op) else op, bound.eval(env).toExpression))
    case _ => None

/** Sign analysis of a quadratic between its roots.
 *
 *  Requires numeric coefficients: the answer is a *sign chart*, and ordering the roots is
 *  what builds it, so a symbolic root that cannot be placed on the line makes the whole
 *  analysis unavailable rather than merely approximate.
 */
private def quadraticTier(c0: _Expression, c1: _Expression, c2: _Expression, op: CompareOp,
                          v: _Variable, env: Environment): Option[_Expression] =
  for
    a0 <- foldReal(c0, env)
    a1 <- foldReal(c1, env)
    a2 <- foldReal(c2, env)
    if math.abs(a2) > RationalEps
    // Normalise to an upward parabola; multiplying through by -1 flips the relation.
    (b0, b1, b2, o) = if a2 < 0 then (-a0, -a1, -a2, flip(op)) else (a0, a1, a2, op)
    result <- quadraticShape(b0, b1, b2, o, v, env)
  yield result

/** Builds the solution of `b2*v^2 + b1*v + b0 <op> 0` for `b2 > 0`. */
private def quadraticShape(b0: Double, b1: Double, b2: Double, op: CompareOp,
                           v: _Variable, env: Environment): Option[_Expression] =
  val disc  = b1 * b1 - 4 * b2 * b0
  val scale = math.max(1.0, math.abs(b1 * b1) + math.abs(4 * b2 * b0))
  val eps   = RationalEps * scale
  if disc < -eps then Some(noRealRoots(op))
  else if disc <= eps then Some(doubleRoot(-b1 / (2 * b2), op, v))
  else
    // Stable roots: the textbook form cancels catastrophically once b1^2 dominates 4*b2*b0
    // (issue 1.1).  Same reasoning as the equality solver's quadratic branch.
    val q  = -(b1 + math.signum(b1) * math.sqrt(disc)) / 2
    val r1 = if b1 == 0.0 then -math.sqrt(disc) / (2 * b2) else q / b2
    val r2 = if b1 == 0.0 then math.sqrt(disc) / (2 * b2) else b0 / q
    val (lo, hi) = if r1 <= r2 then (r1, r2) else (r2, r1)
    Some(twoRoots(lo, hi, op, v))

/** An upward parabola with no real roots is strictly positive everywhere. */
private def noRealRoots(op: CompareOp): _Expression = op match
  case CompareOp.Gt | CompareOp.Ge | CompareOp.Ne => _Bool(true)
  case CompareOp.Lt | CompareOp.Le                => _Bool(false)

/** An upward parabola touching zero at `r`: positive everywhere else. */
private def doubleRoot(r: Double, op: CompareOp, v: _Variable): _Expression = op match
  case CompareOp.Gt | CompareOp.Ne => _Comparison(v, CompareOp.Ne, _Number(r))
  case CompareOp.Ge                => _Bool(true)
  case CompareOp.Lt                => _Bool(false)
  case CompareOp.Le                => _Equation(v, _Number(r))

/** An upward parabola with distinct roots `lo < hi`: negative strictly between them. */
private def twoRoots(lo: Double, hi: Double, op: CompareOp, v: _Variable): _Expression =
  val l = _Number(lo)
  val h = _Number(hi)
  op match
    case CompareOp.Gt => Or(_Comparison(v, CompareOp.Lt, l), _Comparison(v, CompareOp.Gt, h))
    case CompareOp.Ge => Or(_Comparison(v, CompareOp.Le, l), _Comparison(v, CompareOp.Ge, h))
    case CompareOp.Lt => And(_Comparison(v, CompareOp.Gt, l), _Comparison(v, CompareOp.Lt, h))
    case CompareOp.Le => And(_Comparison(v, CompareOp.Ge, l), _Comparison(v, CompareOp.Le, h))
    case CompareOp.Ne => And(_Comparison(v, CompareOp.Ne, l), _Comparison(v, CompareOp.Ne, h))

/** The reversed relation, which is what multiplying through by a negative produces. */
private def flip(op: CompareOp): CompareOp = op match
  case CompareOp.Lt => CompareOp.Gt
  case CompareOp.Gt => CompareOp.Lt
  case CompareOp.Le => CompareOp.Ge
  case CompareOp.Ge => CompareOp.Le
  case CompareOp.Ne => CompareOp.Ne

/** Evaluates `e` to a finite real number, or `None`. */
private def foldReal(e: _Expression, env: Environment): Option[Double] = e.eval(env) match
  case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
  case _                                              => None


// ── Rational tier ─────────────────────────────────────────────────────────────
//
// The sign of `N(v)/D(v)` changes at the roots of `N` *and* at the roots of `D` -- the
// second being poles, where the function is not merely zero but undefined. A root-based
// analysis that ignored them would be wrong rather than incomplete, which is why the old
// 6.23 entry deferred this tier. It does not have to be deferred: `polyRoots` locates both
// sets, so the critical points can be enumerated exactly for a rational function. Only the
// *transcendental* case genuinely needs the domain analysis of issue 6.13.

/** Solves `N(v)/D(v) <op> 0` by sign chart over the critical points. */
private def rationalTier(diff: _Expression, op: CompareOp,
                         v: _Variable, env: Environment): Option[_Expression] =
  for
    (num, den) <- asRational(diff, v, env)
    if polyDegree(den) >= 1 && polyDegree(num) >= 0
    zeros  = realRootsOf(num)
    poles  = realRootsOf(den)
    if zeros.nonEmpty || poles.nonEmpty
    result <- signChart(num, den, zeros, poles, op, v)
  yield result

/** Real roots of a coefficient vector, ascending; empty when it has none or is constant. */
private def realRootsOf(cs: Vector[Double]): Vector[Double] =
  if polyDegree(cs) < 1 then Vector.empty
  else polyRoots(cs).fold(Vector.empty)(_.collect { case _Number(d) if !d.isNaN => d }.sorted)

/** Tests one sample point per open interval and unions those that satisfy the relation.
 *
 *  **Strict relations only.**  A non-strict relation also admits every zero of the numerator,
 *  and such a zero need not touch any satisfied interval: `(x-1)^2/(x-2) >= 0` holds on
 *  `x > 2` *and* at the isolated point `x = 1`.  Rendering that from an interval chart means
 *  emitting one-point pieces and de-duplicating them against the interval endpoints, which is
 *  easy to get subtly wrong — so this tier answers `<`, `>` and `!=`, and leaves `<=` / `>=`
 *  on a genuine rational function symbolic.  Polynomials are unaffected: they are handled by
 *  [[polynomialTier]] and never reach here.
 */
private def signChart(num: Vector[Double], den: Vector[Double],
                      zeros: Vector[Double], poles: Vector[Double],
                      op: CompareOp, v: _Variable): Option[_Expression] =
  val critical = (zeros ++ poles).sorted.distinct
  if op == CompareOp.Le || op == CompareOp.Ge || critical.isEmpty then None
  else
    val pieces = sampleBetween(critical).zipWithIndex
      .filter((x, _) => holdsAt(num, den, x, op))
      .map((_, i) => intervalPiece(critical, i, v))
    if pieces.isEmpty then Some(_Bool(false))
    else if pieces.size == critical.size + 1 then Some(_Bool(true))
    else Some(pieces.reduce((a, b) => Or(a, b)))

/** One interior point per open interval defined by the critical points, plus the two tails. */
private def sampleBetween(critical: Vector[Double]): Vector[Double] =
  val head = critical.head - 1.0
  val tail = critical.last + 1.0
  val mids = critical.sliding(2).collect { case Vector(a, b) => (a + b) / 2 }.toVector
  (head +: mids) :+ tail

/** Whether `N(x)/D(x) <op> 0` holds at the sample point `x`. */
private def holdsAt(num: Vector[Double], den: Vector[Double], x: Double, op: CompareOp): Boolean =
  val d = polyEvalLocal(den, x)
  if math.abs(d) < RationalEps then false
  else
    val value = polyEvalLocal(num, x) / d
    op match
      case CompareOp.Lt => value < 0
      case CompareOp.Le => value < 0
      case CompareOp.Gt => value > 0
      case CompareOp.Ge => value > 0
      case CompareOp.Ne => value != 0

/** Renders the `i`-th open interval of the sign chart as a relation.
 *
 *  Every endpoint is strict: the chart is only built for strict relations, and a critical
 *  point is either a zero (excluded by `<`/`>`) or a pole (excluded by being undefined).
 */
private def intervalPiece(critical: Vector[Double], i: Int, v: _Variable): _Expression =
  if i == 0 then _Comparison(v, CompareOp.Lt, _Number(critical.head))
  else if i == critical.size then _Comparison(v, CompareOp.Gt, _Number(critical.last))
  else And(_Comparison(v, CompareOp.Gt, _Number(critical(i - 1))),
           _Comparison(v, CompareOp.Lt, _Number(critical(i))))

/** Reads `e` as a ratio of two numeric-coefficient polynomials in `v`.
 *
 *  Combines fractions structurally, so `1/x - 2` becomes `(1 - 2x)/x` rather than being
 *  rejected for not already being a single `Ratio`.
 */
private def asRational(e: _Expression, v: _Variable,
                       env: Environment): Option[(Vector[Double], Vector[Double])] = e match
  case Ratio(a, b) =>
    for (na, da) <- asRational(a, v, env); (nb, db) <- asRational(b, v, env)
    yield (mulLocal(na, db), mulLocal(da, nb))
  case Product(a, b) =>
    for (na, da) <- asRational(a, v, env); (nb, db) <- asRational(b, v, env)
    yield (mulLocal(na, nb), mulLocal(da, db))
  case Sum(a, b) =>
    for (na, da) <- asRational(a, v, env); (nb, db) <- asRational(b, v, env)
    yield (addLocal(mulLocal(na, db), mulLocal(nb, da)), mulLocal(da, db))
  case _ =>
    numericCoeffsOf(e, v, env).map(cs => (cs, Vector(1.0)))

/** `collect` plus a fold of every coefficient to a number; `None` if any stays symbolic. */
private def numericCoeffsOf(e: _Expression, v: _Variable,
                            env: Environment): Option[Vector[Double]] =
  collect(e, v).flatMap { cs =>
    cs.foldRight(Option(Vector.empty[Double])) { (c, acc) =>
      for tail <- acc; d <- foldReal(c, env) yield d +: tail
    }
  }

// Dense-coefficient arithmetic, kept local to the rational tier: these have exactly one
// caller, and `Polynomial.scala` earns a helper when there is a second one (the 3.1 lesson).
private def addLocal(a: Vector[Double], b: Vector[Double]): Vector[Double] =
  if a.isEmpty then b else if b.isEmpty then a
  else Vector.tabulate(math.max(a.size, b.size)) { i =>
    a.applyOrElse(i, (_: Int) => 0.0) + b.applyOrElse(i, (_: Int) => 0.0)
  }

private def mulLocal(a: Vector[Double], b: Vector[Double]): Vector[Double] =
  if a.isEmpty || b.isEmpty then Vector.empty
  else
    val out = Array.fill(a.size + b.size - 1)(0.0)
    for i <- a.indices; j <- b.indices do out(i + j) += a(i) * b(j)
    out.toVector

private def polyEvalLocal(cs: Vector[Double], x: Double): Double =
  cs.foldRight(0.0)((c, acc) => acc * x + c)
