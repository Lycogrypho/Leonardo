package it.grypho.scala.leonardo
package scalar

import core.*
import scala.math


/** Four-tier limit engine: lim_{`v` → `point`[`dir`]} `e`.
 *
 *  - Tier 1 — **direct substitution**: works for continuous functions at non-singular points.
 *  - Tier 2 — **[[https://en.wikipedia.org/wiki/L%27H%C3%B4pital%27s_rule L'Hôpital's rule]]** (≤ 5 steps): applied when a `Ratio` evaluates to the
 *    `0/0` or `∞/∞` indeterminate form.  Also detects the `c/0` form and returns `±∞`
 *    based on the direction of approach of the denominator.
 *  - Tier 3 — **structural rules at ±∞**: polynomial rationals via [[collect]]
 *    (`Normalize.scala`), elementary functions (`exp`, `ln`, `atan`, `sin`/`cos`,
 *    and constant-base powers `b^f(v)`) handled by shape.
 *  - Tier 4 — **epsilon perturbation** for one-sided limits that survived all other tiers.
 *
 *  Returns `_Limit(e, v, point, dir)` unchanged when no rule applies (fixpoint convention).
 */
private val MaxHopitalSteps = Limit.MaxHopitalSteps.default.toInt  // the default; evalLimit reads env.limits (F_0124)

/** Evaluates lim_{`v` → `point`[`dir`]} `e`, returning the limit as an expression.
 *
 *  When `point` folds to a concrete `_Number`, the four-tier engine is applied.
 *  When `point` is symbolic the node stays symbolic.
 *
 *  @param e     the expression whose limit to evaluate
 *  @param v     the approach variable
 *  @param point the limit point (may be finite or `±∞`)
 *  @param dir   the direction of approach (`Both`, `FromRight`, or `FromLeft`)
 *  @param env   environment for resolving free variables other than `v`
 *  @return the limit value as a `_Number`, or the unchanged `_Limit` node when it cannot be determined
 */
def evalLimit(e: _Expression, v: _Variable, point: _Expression, dir: LimitDir, env: Environment): _Expression =
  // A decline is the call as written (F_0276): the tiers stop on whatever form they reached — a
  // quotient L'Hôpital's rule differentiated, the point read as a `Double` — which the echo showed
  // and `_Limit.eval` then evaluated again as a new limit.
  limitTiers(e, v, point, dir, env) match
    // F_0286: the limit algebra answers what every sampling tier declined — a free parameter, growth
    // at infinity, an infinite one-sided limit.  Consulted last, so no earlier answer changes.
    case _: _Limit => limitAlgebra(e, v, point, dir, env).fold(_Limit(e, v, point, dir))(_._1)
    case answer    => answer

/** The tiers behind [[evalLimit]], which normalises a decline to the call as written. */
private def limitTiers(e: _Expression, v: _Variable, point: _Expression, dir: LimitDir, env: Environment): _Expression =
  point.eval(env) match
    // An exact point is tried exactly first (F_0210): `limit(x^2, x, 1/3)` answered 0.11111.
    // Placed before the widening `_Number` arm, which would read the rational as a Double.
    // A surd point too (F_0263): `limit(x^2, x, 2^(1/2))` answered `2.0`.
    case Right(r @ _Number(d)) if _Surd.isExact(r) =>
      exactLimit(e, v, r, dir, env, env.limits.count(Limit.MaxHopitalSteps))
        .getOrElse(orComplex(limitFinite(e, v, d, dir, env, env.limits.count(Limit.MaxHopitalSteps)), e, v, d, dir, env))
    case Right(_Number(p)) =>
      if p.isInfinite then limitInfinity(e, v, p, env)
      else orComplex(limitFinite(e, v, p, dir, env, env.limits.count(Limit.MaxHopitalSteps)), e, v, p, dir, env)
    case _ => _Limit(e, v, point, dir)   // symbolic point — stay symbolic

/** The limit at an exact point, exactly, or `None` to leave it to the `Double` tiers (F_0210):
 *  direct substitution when it gives an exact value and no step jumps there, and L'Hôpital at an
 *  exact `0/0`.  The derivatives are taken in the expression's own tier, so an exact numerator
 *  stays exact through each step.  Continuity is asked of the sides `dir` names (F_0332: the
 *  one-sided limit of `floor` at its jump is the value from that side, which a two-sided check refused,
 *  so the `Double` tier answered `1.0` for `1`).  Anything else — a pole, a decimal — falls through
 *  unchanged, so no answer that was not exact before is changed. */
private def exactLimit(e: _Expression, v: _Variable, p: _Value, dir: LimitDir, env: Environment,
                       stepsLeft: Int): Option[_Expression] =
  def at(x: _Expression): Option[_Value] = x.eval(env.withBinding(v.variable, p)).toOption
  def zero(x: Option[_Value]): Boolean = x.exists(y => _Surd.signum(y).contains(0))
  val pd = p match { case _Number(d) => d; case _ => Double.NaN }
  if jumpsAt(e, v, pd, env) then None
  else at(e) match
    // A surd answer too (F_0263): `limit(x^(1/2), x, 2)` answered `1.41421`.  Only where the
    // expression is continuous (F_0276): `sec(x) - tan(x)` at the exact reading of `pi/2` cancels two
    // readings of size 1e43 and answered `-4.8E42`.
    case Some(r @ _Number(rd)) if _Surd.isExact(r) && continuousAt(e, v, pd, rd, dir, env) => Some(r)
    case _ => e match
      case Ratio(num, den) if stepsLeft > 0 && zero(at(num)) && zero(at(den)) =>
        exactLimit(Ratio(derive(num, v), derive(den, v)), v, p, dir, env, stepsLeft - 1)
      case _ => None

/** The principal complex value at `p` when the real tiers declined (F_0297, the owner's decision):
 *  `x^(1/3)` at `-8` is `1 + 1.732i`, the value `Power` computes there, and is the limit when the samples
 *  beside `p` approach it as [[continuousAt]] asks of a real value.  Only a genuinely complex value: a
 *  real one the real tiers declined stays declined. */
private def orComplex(real: _Expression, e: _Expression, v: _Variable, p: Double, dir: LimitDir,
                      env: Environment): _Expression = real match
  case declined: _Limit =>
    def at(x: Double): Option[(Double, Double)] =
      e.eval(env.withBinding(v.variable, _Number(x))) match
        case Right(c: _Complex) => _Complex.parts(c).filter((a, b) => a.isFinite && b.isFinite)
        case Right(_Number(d)) if d.isFinite => Some((d, 0.0))
        case _ => None
    def dist(a: (Double, Double), b: (Double, Double)): Double = math.hypot(a._1 - b._1, a._2 - b._2)
    e.eval(env.withBinding(v.variable, _Number(p))) match
      case Right(c: _Complex) if !jumpsAt(e, v, p, env) =>
        _Complex.parts(c).filter((a, b) => a.isFinite && b.isFinite).filter { fp =>
          val eps = math.max(1e-8, math.abs(p) * 1e-6)
          def side(s: Double): Boolean = (at(p + s * eps), at(p + s * eps * 1e-3)) match
            case (Some(y1), Some(y2)) =>
              dist(y2, fp) <= 1e-9 * math.hypot(fp._1, fp._2) || dist(y2, fp) <= 0.5 * dist(y1, fp)
            case _ => false
          dir match
            case LimitDir.FromRight => side(1.0)
            case LimitDir.FromLeft  => side(-1.0)
            case LimitDir.Both      => side(1.0) && side(-1.0)
        }.fold[_Expression](declined)(_ => c)
      case _ => declined
  case answer => answer

/** Evaluates `e` with `v` bound to `p`; returns `Some(d)` iff the result is a concrete `_Number`. */
private def numericAt(e: _Expression, v: _Variable, p: Double, env: Environment): Option[Double] =
  e.eval(env.withBinding(v.variable, _Number(p))) match
    case Right(_Number(d)) => Some(d)
    case _                 => None

@annotation.tailrec
private def limitFinite(
  e: _Expression, v: _Variable, p: Double, dir: LimitDir, env: Environment, stepsLeft: Int
): _Expression =
  // Tier 1: direct substitution — valid only where `e` is continuous at `p`, so not at a jump
  // (F_0192: `limit(step(x), x, 0, -)` read the value at 0, `1`, where the limit is `0`).
  // Nor where the value disagrees with the values beside it (F_0276): `0/sin(pi)` is `0` because
  // `sin(pi)` is `1.2e-16`, and `limit((x^3 - exp(x - 1))/sin(pi*x), x, 1)` answered `0` for `-2/pi`.
  numericAt(e, v, p, env) match
    case Some(d) if !d.isNaN && !jumpsAt(e, v, p, env) && continuousAt(e, v, p, d, dir, env) => _Number(d)
    case _ =>
      // Tier 2: special Ratio handling
      e match
        case Ratio(num, den) if stepsLeft > 0 =>
          val nv = numericAt(num, v, p, env)
          val dv = numericAt(den, v, p, env)
          if (nv.contains(0.0) && dv.contains(0.0)) ||
             (nv.exists(_.isInfinite) && dv.exists(_.isInfinite)) then
            // 0/0 or ∞/∞: apply L'Hôpital
            limitFinite(Ratio(derive(num, v), derive(den, v)), v, p, dir, env, stepsLeft - 1)
          else if nv.exists(n => n.isFinite && n != 0.0) && dv.contains(0.0) then
            // c/0: limit is ±∞; sign determined by direction of denominator approach
            nv.fold(limitEpsilon(e, v, p, dir, env))(n => cOverZero(e, den, v, p, dir, env, math.signum(n)))
          else
            limitEpsilon(e, v, p, dir, env)
        case _ => limitEpsilon(e, v, p, dir, env)

/** Whether a step inside `e` switches at `p`: its argument vanishes there, so `e` may jump, and
 *  the one-sided tiers must decide rather than the value at the point (F_0192). */
private def jumpsAt(e: _Expression, v: _Variable, p: Double, env: Environment): Boolean = e match
  case _Heaviside(arg) if numericAt(arg, v, p, env).exists(_ == 0.0) => true
  case other                                                         => other.children.exists(jumpsAt(_, v, p, env))

/** Whether `fp`, the value substitution gave at `p`, is the limit there (F_0276): on each side the
 *  direction asks for, the samples at `ε` and `ε/1000` agree with it or approach it, the nearer one at
 *  most half as far.
 *
 *  Substitution is right only where the expression is continuous, and nothing else checked it: `0/sin(π)`
 *  is `0` because `sin(π)` is a reading of zero, `1/sin(π)` is `8.2E15`, and a step of L'Hôpital's rule
 *  can meet `0 · undefined`, which `Product`'s zero short-circuit answers `0`.  Each of those disagrees
 *  with every value beside it.  A side the expression is not defined on (a domain boundary) cannot
 *  refute the value, so it is skipped.
 *
 *  @param e   the expression
 *  @param v   the limit variable
 *  @param p   the point
 *  @param fp  the value at `p`
 *  @param dir the sides to check
 *  @param env the bindings
 *  @return `false` when a side's samples neither agree with `fp` nor approach it
 */
private def continuousAt(e: _Expression, v: _Variable, p: Double, fp: Double, dir: LimitDir, env: Environment): Boolean =
  val eps = math.min(math.max(1e-8, math.abs(p) * 1e-6), smallScale(e, v, env) * FeatureMargin)
  def side(s: Double): Boolean =
    (numericAt(e, v, p + s * eps, env), numericAt(e, v, p + s * eps * 1e-3, env)) match
      case (Some(y1), Some(y2)) =>
        y1.isFinite && y2.isFinite &&
          (math.abs(y2 - fp) <= 1e-9 * math.max(math.abs(y2), math.abs(fp)) ||
            math.abs(y2 - fp) <= 0.5 * math.abs(y1 - fp))
      case _ => true
  dir match
    case LimitDir.FromRight => side(1.0)
    case LimitDir.FromLeft  => side(-1.0)
    case LimitDir.Both      => side(1.0) && side(-1.0)

/** The sign of `den` just beside its zero at `p`, on the side `s` (`±1`), from the first of its
 *  derivatives that does not vanish there (F_0276): of order `k`, the sign is the derivative's on the
 *  right and `(−1)ᵏ` times it on the left.  `None` when a derivative does not evaluate or none of the first
 *  four is non-zero. */
private def signFromDerivatives(den: _Expression, v: _Variable, p: Double, s: Double, env: Environment): Option[Double] =
  @annotation.tailrec
  def go(d: _Expression, k: Int): Option[Double] =
    if k > 4 then None
    else
      val dk = derive(d, v)
      numericAt(dk, v, p, env) match
        case Some(x) if !x.isFinite => None
        case Some(x) if x == 0.0    => go(dk, k + 1)
        case Some(x)                => Some(math.signum(x) * (if s < 0 && k % 2 == 1 then -1.0 else 1.0))
        case None                   => None
  go(den, 1)

/** Handles `c / (something→0)`: returns `signN * sign(den near p) * ∞`, checking direction.
 *
 *  The sign comes from [[signFromDerivatives]], checked against a sample beside the point; a zero,
 *  undecided or contradicted sign declines (F_0276).  A sample alone read `0` beside `−5` for
 *  `r² + 10r + 25` (the terms cancel), and `−∞ · 0` printed `NaN`. */
private def cOverZero(
  e: _Expression, den: _Expression, v: _Variable, p: Double,
  dir: LimitDir, env: Environment, signN: Double
): _Expression =
  val eps = math.min(math.max(1e-8, math.abs(p) * 1e-8), smallScale(e, v, env) * FeatureMargin)
  def signBeside(s: Double): Option[Double] =
    val sampled = numericAt(den, v, p + s * eps, env).map(math.signum).filter(x => x != 0.0 && !x.isNaN)
    (signFromDerivatives(den, v, p, s, env), sampled) match
      case (Some(d), Some(x)) if d != x => None
      case (Some(d), _)                 => Some(d)
      case (None, sx)                   => sx
  def signedInf(sgn: Option[Double]): _Expression =
    sgn.map(d => _Number(signN * d * Double.PositiveInfinity)).getOrElse(_Limit(e, v, _Number(p), dir))
  dir match
    case LimitDir.FromRight => signedInf(signBeside(1.0))
    case LimitDir.FromLeft  => signedInf(signBeside(-1.0))
    case LimitDir.Both =>
      (signBeside(-1.0), signBeside(1.0)) match
        case (Some(l), Some(r)) if l == r => _Number(signN * l * Double.PositiveInfinity)
        case _                            => _Limit(e, v, _Number(p), dir)   // sign flip → DNE

/** Tier 4: evaluates at three points close to `p` from the requested direction, at `10ε`, `ε` and
 *  `ε/1000`.  For one-sided limits, reports the mean of the two nearest when all three agree.
 *  For two-sided limits, checks that the left and right estimates both converge
 *  and agree with each other; stays symbolic when any check fails.
 *
 *  **Three, not two** (F_0276): where the expression loses every digit near the point, the two nearest
 *  samples collapse to the same constant and agree — `x^(-2) - (x^(-2) - 1)` is `0` at `10⁻⁸` and `10⁻¹¹`,
 *  `(sin(x)/x)^(1/x^2)` is `1` — and the limit answered that constant.  At `10ε` the digits are still
 *  there, so the collapse disagrees with it and the limit declines.
 */
private def limitEpsilon(
  e: _Expression, v: _Variable, p: Double, dir: LimitDir, env: Environment
): _Expression =
  val eps = math.min(math.max(1e-8, math.abs(p) * 1e-6), smallScale(e, v, env) * FeatureMargin)
  def estimate(s: Double): Option[Double] =
    Seq(10.0, 1.0, 1e-3).map(k => numericAt(e, v, p + s * eps * k, env)) match
      case Seq(Some(y0), Some(y1), Some(y2))
          if Seq(y0, y1, y2).forall(_.isFinite) && converged(y0, y1, 1e-5) && converged(y1, y2, 1e-5) &&
             converged(y0, y2, 1e-5) => Some((y1 + y2) / 2)
      case _ => None
  dir match
    case LimitDir.FromRight =>
      estimate(1.0).map(_Number(_)).getOrElse(_Limit(e, v, _Number(p), dir))
    case LimitDir.FromLeft  =>
      estimate(-1.0).map(_Number(_)).getOrElse(_Limit(e, v, _Number(p), dir))
    case LimitDir.Both =>
      val rEst = estimate(1.0)
      val lEst = estimate(-1.0)
      (lEst, rEst) match
        case (Some(l), Some(r)) if converged(l, r, 1e-5) =>
          _Number((l + r) / 2)
        case _ => _Limit(e, v, _Number(p), dir)

/** Tier 3: structural evaluation at ±∞ by recursing over the AST shape. */
private def limitInfinity(e: _Expression, v: _Variable, p: Double, env: Environment): _Expression =
  val posInf = p.isPosInfinity

  def go(ex: _Expression): _Expression = ex match
    case _ if !dependsOn(ex, v) => ex.eval(env).toExpression   // constant w.r.t. v
    case vv: _Variable if vv.variable == v.variable => _Number(p)   // v itself → ±∞

    case Atan(inner) =>
      go(inner) match
        case _Number(d) if d.isPosInfinity =>  _Number(math.Pi / 2)
        case _Number(d) if d.isNegInfinity => _Number(-math.Pi / 2)
        case _ => _Limit(ex, v, _Number(p), LimitDir.Both)

    case Exp(inner) =>
      go(inner) match
        case _Number(d) if d.isPosInfinity  => _Number(Double.PositiveInfinity)
        case _Number(d) if d.isNegInfinity  => _Number(0.0)
        case _ => _Limit(ex, v, _Number(p), LimitDir.Both)

    case Ln(inner) =>
      go(inner) match
        case _Number(d) if d.isPosInfinity  => _Number(Double.PositiveInfinity)
        case _ => _Limit(ex, v, _Number(p), LimitDir.Both)

    // Trig functions oscillate at ±∞ — no limit exists in general.
    case Sin(_) | Cos(_) | Tg(_) => _Limit(ex, v, _Number(p), LimitDir.Both)

    case Ratio(num, den) =>
      (collect(num, v), collect(den, v)) match
        case (Some(pcs), Some(qcs)) =>
          // Polynomial rational function: compare degrees of numerator vs denominator.
          val degP = pcs.length - 1
          val degQ = qcs.length - 1
          // The limit is built in the input's tier (F_0210): an exact rational function at ∞
          // answered `0.0` and `0.66667`.
          if degP < degQ then _Rational.literalLike(0, ex, env)
          else if degP == degQ then
            (pcs.last.eval(env).toExpression, qcs.last.eval(env).toExpression) match
              case (lp: _Rational, lq: _Rational) if lq.signum != 0 =>
                lp.divide(lq).getOrElse(_Number(lp.toDouble / lq.toDouble))
              case (_Number(lp), _Number(lq)) if lq != 0.0 => _Number(lp / lq)
              case (lp, lq) => Ratio(lp, lq)   // symbolic leading coefficients
          else
            (pcs.last.eval(env).toExpression, qcs.last.eval(env).toExpression) match
              case (_Number(lp), _Number(lq)) if lq != 0.0 =>
                val diff    = degP - degQ
                val baseSign = math.signum(lp / lq)
                // At +∞ the sign of x^diff is positive; at -∞ it is (-1)^diff.
                val infSign = if posInf then baseSign
                              else baseSign * (if diff % 2 == 0 then 1.0 else -1.0)
                _Number(infSign * Double.PositiveInfinity)
              case _ => numericInfFallback(ex, v, p, env)
        case _ =>
          // Non-polynomial: try recursive limits on numerator and denominator.
          (go(num), go(den)) match
            case (_Number(na), _Number(db)) if !db.isNaN && db != 0.0 =>
              val r = na / db
              if r.isNaN then numericInfFallback(ex, v, p, env) else _Number(r)
            case _ => numericInfFallback(ex, v, p, env)

    // Two finite exact limits combine exactly (F_0263): `limit(1/3 + 1/x, x, inf)` answered 0.33333.
    case Sum(a, b) =>
      (go(a), go(b)) match
        case (la, lb) if exactFinite(la) && exactFinite(lb) => Sum(la, lb).eval(env).toExpression
        case (_Number(da), _Number(db)) =>
          val s = da + db
          if s.isNaN then _Limit(ex, v, _Number(p), LimitDir.Both) else _Number(s)
        case _ => _Limit(ex, v, _Number(p), LimitDir.Both)

    case Product(a, b) =>
      (go(a), go(b)) match
        case (la, lb) if exactFinite(la) && exactFinite(lb) => Product(la, lb).eval(env).toExpression
        case (_Number(da), _Number(db)) =>
          val s = da * db
          if s.isNaN then _Limit(ex, v, _Number(p), LimitDir.Both) else _Number(s)
        case _ => _Limit(ex, v, _Number(p), LimitDir.Both)

    // Constant base, v-dependent exponent: b^f(v) as v → ±∞.
    // b > 1: f→+∞ ⟹ +∞, f→-∞ ⟹ 0.  0 < b < 1: reversed. b == 1: always 1.
    // Negative and zero bases stay symbolic (complex / degenerate territory).
    case Power(base, exp) if !dependsOn(base, v) =>
      base.eval(env) match
        case Right(_Number(b)) if b > 0.0 =>
          go(exp) match
            case _Number(d) if d.isPosInfinity =>
              if b > 1.0      then _Number(Double.PositiveInfinity)
              else if b < 1.0 then _Number(0.0)
              else _Number(1.0)
            case _Number(d) if d.isNegInfinity =>
              if b > 1.0      then _Number(0.0)
              else if b < 1.0 then _Number(Double.PositiveInfinity)
              else _Number(1.0)
            case _Number(d) =>
              val r = math.pow(b, d)
              if r.isFinite then _Number(r) else numericInfFallback(ex, v, p, env)
            case _ => numericInfFallback(ex, v, p, env)
        case _ => numericInfFallback(ex, v, p, env)

    case Power(base, _Number(n)) =>
      go(base) match
        case _Number(d) =>
          val r = math.pow(d, n)
          if r.isNaN then _Limit(ex, v, _Number(p), LimitDir.Both) else _Number(r)
        case _ => _Limit(ex, v, _Number(p), LimitDir.Both)

    case _ => numericInfFallback(ex, v, p, env)

  go(e)

/** Whether a limit is an exact value: a rational or a surd, never an infinity (which is a
 *  `Double`), so combining two of them exactly is sound. */
private def exactFinite(x: _Expression): Boolean = _Surd.isExact(x)

/** Numeric fallback at ∞: evaluates at two large values and checks convergence.  The samples sit
 *  at least six decades beyond the expression's largest scale (F_0257): at the fixed `10¹⁰` and
 *  `10¹⁵`, `limit(1e30/(abs(x) + 1e30), x, inf)` answered `1.0` for a limit `0`. */
private def numericInfFallback(e: _Expression, v: _Variable, p: Double, env: Environment): _Expression =
  val sign  = if p.isPosInfinity then 1 else -1
  val shift = math.min(math.max(1.0, largeScale(e, v, env) / FeatureMargin / 1e10), 1e290)
  // A third sample at an incommensurate point (F_0276): `(-1)^(n - 1)` has the same parity at
  // `1e10` and `1e15`, and answered `-1.0` for a limit that does not exist.
  Seq(1e10, 1e10 * GoldenRatio, 1e15).map(x => numericAt(e, v, sign * x * shift, env)) match
    case Seq(Some(y0), Some(y1), Some(y2))
        if Seq(y0, y1, y2).forall(_.isInfinite) && Seq(y0, y1).forall(y => math.signum(y) == math.signum(y2)) =>
      _Number(y2)
    case Seq(Some(y0), Some(y1), Some(y2))
        if Seq(y0, y1, y2).forall(_.isFinite) && converged(y0, y2, 1e-6) && converged(y1, y2, 1e-6) =>
      _Number(y2)
    case _ => _Limit(e, v, _Number(p), LimitDir.Both)

/** The golden ratio, irrational, so a sample at `φ·10¹⁰` shares no period with one at `10¹⁰`. */
private val GoldenRatio = (1 + math.sqrt(5)) / 2

/** How far beyond a scale written in the expression the numeric tiers sample: six decades
 *  (F_0257).  `exp(-1e-20/x)` changes only below `x ≈ 10⁻²⁰`, so its limit at `0⁺` read `1.0` from
 *  samples at `10⁻⁸` and `10⁻¹¹`; `1/(x*(x - 1e-10))` changes sign at `10⁻¹⁰`, so `±10⁻⁸` saw
 *  one sign and answered `inf` for a limit that does not exist.  The default windows are kept
 *  whenever the expression's scales lie inside them, so every other answer is unchanged. */
private val FeatureMargin = 1e-6

/** The magnitudes of the constants `e` is written with, and of the numbers its other free names
 *  are bound to: the scales at which it can change. */
private def scalesOf(e: _Expression, v: _Variable, env: Environment): Iterable[Double] =
  def leaves(x: _Expression): Iterable[Double] = x match
    case _Number(d)              => Seq(math.abs(d))
    case w: _Variable if w != v  => env.get(w.variable).collect { case _Number(d) => math.abs(d) }
    case other                   => other.children.flatMap(leaves)
  leaves(e).filter(m => m > 0.0 && m.isFinite)

/** The smallest scale in `e`: `min(|c|, 1/|c|)` over its constants, `1` when it has none — a
 *  large constant multiplying `x` makes a feature at `1/c` as surely as a small one does at `c`. */
private def smallScale(e: _Expression, v: _Variable, env: Environment): Double =
  scalesOf(e, v, env).map(m => math.min(m, 1.0 / m)).minOption.getOrElse(1.0)

/** The largest scale in `e`: `max(|c|, 1/|c|)` over its constants, `1` when it has none. */
private def largeScale(e: _Expression, v: _Variable, env: Environment): Double =
  scalesOf(e, v, env).map(m => math.max(m, 1.0 / m)).maxOption.getOrElse(1.0)

/** Whether two samples agree to `tol` **of their own size** (F_0211).
 *
 *  It was `tol·(1 + |y|)`, absolute below unit scale: `limit(sin(x)/1e7, x, inf)` and
 *  `limit(sin(1/x)*1e-7, x, 0, +)` answered `0.0`, because two samples of an oscillation of size
 *  `10⁻⁷` differ by less than `10⁻⁶`.  **The price, accepted**: a genuine limit `0` reached only
 *  through the numeric tiers now declines, since two samples cannot tell a small limit from a small
 *  oscillation; equal samples still agree. */
private def converged(y1: Double, y2: Double, tol: Double): Boolean =
  math.abs(y1 - y2) <= tol * math.max(math.abs(y1), math.abs(y2))
