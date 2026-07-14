package it.grypho.scala.leonardo
package scalar

import core.*
import scala.math


// Limit evaluation: lim_{v → point[dir]} e
//
// Tier 1 — direct substitution: works for continuous functions at non-singular points.
// Tier 2 — L'Hôpital's rule (≤ 5 steps): applied when a Ratio evaluates to the 0/0
//           or ∞/∞ indeterminate form. Also detects the c/0 form and returns ±∞ based
//           on the direction of approach of the denominator.
// Tier 3 — structural rules at ±∞: polynomial rationals via collect (Normalize.scala),
//           elementary functions (exp, ln, atan, sin/cos) handled by shape.
// Tier 4 — epsilon perturbation for one-sided limits that survived all other tiers.
//
// Returns _Limit(e, v, point, dir) unchanged when no rule applies (fixpoint convention).

private val MaxHopitalSteps = 5

def evalLimit(e: _Expression, v: _Variable, point: _Expression, dir: LimitDir, env: Environment): _Expression =
  point.eval(env) match
    case Right(_Number(p)) =>
      if p.isInfinite then limitInfinity(e, v, p, env)
      else limitFinite(e, v, p, dir, env, MaxHopitalSteps)
    case _ => _Limit(e, v, point, dir)   // symbolic point — stay symbolic

// Evaluate e with v bound to p; return Some(d) iff the result is a concrete _Number.
// Ratio.eval / Power.eval return Left for NaN / infinite, so this returns None there.
private def numericAt(e: _Expression, v: _Variable, p: Double, env: Environment): Option[Double] =
  e.eval(env.withBinding(v.variable, _Number(p))) match
    case Right(_Number(d)) => Some(d)
    case _                 => None

@annotation.tailrec
private def limitFinite(
  e: _Expression, v: _Variable, p: Double, dir: LimitDir, env: Environment, stepsLeft: Int
): _Expression =
  // Tier 1: direct substitution
  numericAt(e, v, p, env) match
    case Some(d) if !d.isNaN => _Number(d)
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

// c / (something→0): return signN * sign(den near p) * ∞, checking direction.
private def cOverZero(
  e: _Expression, den: _Expression, v: _Variable, p: Double,
  dir: LimitDir, env: Environment, signN: Double
): _Expression =
  val eps = math.max(1e-8, math.abs(p) * 1e-8)
  def signedInf(sampleP: Double): _Expression =
    numericAt(den, v, sampleP, env)
      .map(d => _Number(signN * math.signum(d) * Double.PositiveInfinity))
      .getOrElse(_Limit(e, v, _Number(p), dir))
  dir match
    case LimitDir.FromRight => signedInf(p + eps)
    case LimitDir.FromLeft  => signedInf(p - eps)
    case LimitDir.Both =>
      val ls = numericAt(den, v, p - eps, env).map(math.signum)
      val rs = numericAt(den, v, p + eps, env).map(math.signum)
      (ls, rs) match
        case (Some(l), Some(r)) if l == r => _Number(signN * l * Double.PositiveInfinity)
        case _                            => _Limit(e, v, _Number(p), dir)   // sign flip → DNE

// Tier 4: evaluate at two points close to p from the requested direction.
// Reports the average if they converge; stays symbolic otherwise.
private def limitEpsilon(
  e: _Expression, v: _Variable, p: Double, dir: LimitDir, env: Environment
): _Expression =
  dir match
    case LimitDir.Both => _Limit(e, v, _Number(p), dir)
    case _ =>
      val eps = math.max(1e-8, math.abs(p) * 1e-6)
      val (x1, x2) = dir match
        case LimitDir.FromRight => (p + eps, p + eps * 0.001)
        case LimitDir.FromLeft  => (p - eps, p - eps * 0.001)
        case LimitDir.Both      => (p, p)   // unreachable
      (numericAt(e, v, x1, env), numericAt(e, v, x2, env)) match
        case (Some(y1), Some(y2)) if y1.isFinite && y2.isFinite &&
             math.abs(y1 - y2) <= 1e-5 * (1.0 + math.abs(y2)) =>
          _Number((y1 + y2) / 2)
        case _ => _Limit(e, v, _Number(p), dir)

// Structural evaluation at ±∞: recurse over the AST shape.
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
          if degP < degQ then _Number(0.0)
          else if degP == degQ then
            (pcs.last.eval(env).toExpression, qcs.last.eval(env).toExpression) match
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

    case Sum(a, b) =>
      (go(a), go(b)) match
        case (_Number(da), _Number(db)) =>
          val s = da + db
          if s.isNaN then _Limit(ex, v, _Number(p), LimitDir.Both) else _Number(s)
        case _ => _Limit(ex, v, _Number(p), LimitDir.Both)

    case Product(a, b) =>
      (go(a), go(b)) match
        case (_Number(da), _Number(db)) =>
          val s = da * db
          if s.isNaN then _Limit(ex, v, _Number(p), LimitDir.Both) else _Number(s)
        case _ => _Limit(ex, v, _Number(p), LimitDir.Both)

    case Power(base, _Number(n)) =>
      go(base) match
        case _Number(d) =>
          val r = math.pow(d, n)
          if r.isNaN then _Limit(ex, v, _Number(p), LimitDir.Both) else _Number(r)
        case _ => _Limit(ex, v, _Number(p), LimitDir.Both)

    case _ => numericInfFallback(ex, v, p, env)

  go(e)

// Numeric fallback at ∞: evaluate at two large values and check convergence.
private def numericInfFallback(e: _Expression, v: _Variable, p: Double, env: Environment): _Expression =
  val sign = if p.isPosInfinity then 1 else -1
  (numericAt(e, v, sign * 1e10, env), numericAt(e, v, sign * 1e15, env)) match
    case (Some(y1), Some(y2)) if y1.isInfinite && y2.isInfinite && math.signum(y1) == math.signum(y2) =>
      _Number(y1)
    case (Some(y1), Some(y2)) if y1.isFinite && y2.isFinite &&
         math.abs(y1 - y2) <= 1e-6 * (1.0 + math.abs(y2)) =>
      _Number(y2)
    case _ => _Limit(e, v, _Number(p), LimitDir.Both)
