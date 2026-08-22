package it.grypho.scala.leonardo
package scalar

import core.*


/** Highest Taylor order [[taylorSeries]] will expand.
 *
 *  Matches `Expand`'s integer-power cap of 20 and exists for the same reason: the work
 *  grows with the order (each term needs one more differentiation), and a runaway order
 *  should give up rather than hang.  Beyond the cap the `_Taylor` node stays symbolic.
 */
val MaxTaylorOrder: Int = 20


/** Returns `true` when an unevaluated `_Derivative` survives anywhere in `e`.
 *
 *  The Taylor coefficients are built from repeated differentiation, and `derive` returns
 *  the `_Derivative` node itself for anything outside its rule table (`Gamma`, `fact`, an
 *  integral in an unrelated variable, ...).  A series whose coefficients contain such a
 *  node is not a polynomial and is worse than no answer, so its presence is the signal to
 *  stay symbolic -- the same guard `SolveODESymbolic` applies to a surviving `_Integral`.
 */
private def hasDerivative(e: _Expression): Boolean = e match
  case _: _Derivative => true
  case other          => other.children.exists(hasDerivative)


/** Expands `e` as a Taylor polynomial in `v` about `point`, to and including order `order`.
 *
 *  `Σ(k = 0 to order) f⁽ᵏ⁾(point) / k! · (v − point)ᵏ`, built by folding
 *  [[deriveN]] (which is memoised, so the k-th derivative reuses the work of the
 *  (k−1)-th) and instantiating each coefficient at `point` with [[substitute]].  The
 *  result is run through [[simplifyFully]], which collapses the `k = 0` term's
 *  `(v − point)⁰` to `1` and folds the constant coefficients.
 *
 *  `point` need not be numeric: expanding about a symbolic centre `a` is meaningful and
 *  yields a polynomial in `(v − a)` with `f⁽ᵏ⁾(a)` coefficients.
 *
 *  @param e     the expression to expand
 *  @param v     the expansion variable; it appears **free** in the result, so it is not a
 *               binder in the `_Derivative` sense
 *  @param point the centre of the expansion
 *  @param order the highest power retained; must be in `0 .. MaxTaylorOrder`
 *  @return the truncated series, or `None` when the order is out of range or some
 *          coefficient could not be differentiated
 */
def taylorSeries(
    e: _Expression, v: _Variable, point: _Expression, order: Int
): Option[_Expression] =
  if order < 0 || order > MaxTaylorOrder then None
  else
    // foldLeft over an Option accumulator: the first underivable coefficient aborts the
    // whole series rather than yielding a polynomial with a _Derivative inside it.
    val terms = (0 to order).foldLeft(Option(Vector.empty[_Expression])) { (acc, k) =>
      acc.flatMap { built =>
        val dk = deriveN(e, v, k)
        Option.when(!hasDerivative(dk)) {
          val atPoint = substitute(dk, Map(v.variable -> point))
          val shifted = Sum(v, Product(_Number(-1), point))
          built :+ Product(
            Ratio(atPoint, _Number(factorialOf(k.toDouble).getOrElse(Double.NaN))),
            Power(shifted, _Number(k)))
        }
      }
    }
    terms.map(ts => simplifyFully(ts.reduceLeft(Sum.apply)))


/** Expands `e` as a Maclaurin polynomial — the Taylor series about zero.
 *
 *  Sugar over [[taylorSeries]] with `point = 0`, not a separate algorithm; the grammar's
 *  `maclaurin(e, v, n)` desugars to `taylor(e, v, 0, n)` in the same way `log(x)`
 *  desugars to `LogBase(x, 10)`.
 *
 *  @param e     the expression to expand
 *  @param v     the expansion variable
 *  @param order the highest power retained
 *  @return the truncated series, or `None` under the same conditions as [[taylorSeries]]
 */
def maclaurinSeries(e: _Expression, v: _Variable, order: Int): Option[_Expression] =
  taylorSeries(e, v, _Number(0), order)


/** Highest Fourier order [[fourierSeries]] will compute.
 *
 *  Each order costs two definite integrations, so the work is linear in the order but the
 *  constant is large (Simpson uses `max(precision * 200, 100)` intervals per integral).
 *  Capped at the same 20 as [[MaxTaylorOrder]] and `Expand`'s power cap.
 */
val MaxFourierOrder: Int = 20


/** Expands `e` as a truncated Fourier series in `v` over one period, centred on zero.
 *
 *  `a₀/2 + Σ(k = 1 .. order) [ a_k·cos(kωv) + b_k·sin(kωv) ]` with `ω = 2π/T` and
 *
 *  {{{
 *  a_k = (2/T) ∫ f(t)·cos(kωt) dt      b_k = (2/T) ∫ f(t)·sin(kωt) dt
 *  }}}
 *
 *  both taken over `[−T/2, T/2]`.  The coefficients are computed **numerically**, by
 *  handing each integrand to `_DefIntegral` — which means this needs a real
 *  `Environment` (unlike [[taylorSeries]], which is purely symbolic) and that `period`
 *  must be a concrete positive number.
 *
 *  Coefficients are **not** thresholded.  A term that vanishes analytically — every sine
 *  coefficient of an even function, say — comes back at the integrator's noise floor
 *  rather than exactly zero, and is kept.  Chopping it would mean inventing an epsilon
 *  and silently discarding genuinely small coefficients; following the library's rule
 *  that cleanup is a *display* concern, the noise is left visible instead.
 *
 *  @param e      the expression to expand
 *  @param v      the expansion variable; free in the result, as in [[taylorSeries]]
 *  @param period the period `T`; must be finite and strictly positive
 *  @param order  the highest harmonic retained; must be in `0 .. MaxFourierOrder`
 *  @param env    environment used to evaluate the coefficient integrals
 *  @return the truncated series, or `None` when an argument is out of range or some
 *          coefficient integral does not reduce to a number
 */
def fourierSeries(
    e: _Expression, v: _Variable, period: Double, order: Int, env: Environment
): Option[_Expression] =
  if order < 0 || order > MaxFourierOrder then None
  else if !(period > 0.0) || period.isInfinite || period.isNaN then None
  else
    val halfT = period / 2.0
    val omega = 2.0 * math.Pi / period

    /** (2/T) times the integral of `e * weight` over one period, or None. */
    def coefficient(weight: _Expression): Option[Double] =
      _DefIntegral(Product(e, weight), v, _Number(-halfT), _Number(halfT)).eval(env) match
        case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(2.0 / period * d)
        case _                                              => None

    /** The harmonic argument `k*omega*v`. */
    def harmonic(k: Int): _Expression = Product(_Number(k * omega), v)

    // a0/2 is the mean value over the period; the k >= 1 harmonics follow.
    coefficient(_Number(1)).flatMap { a0 =>
      val terms = (1 to order).foldLeft(Option(Vector[_Expression](_Number(a0 / 2.0)))) { (acc, k) =>
        acc.flatMap { built =>
          for ak <- coefficient(Cos(harmonic(k))); bk <- coefficient(Sin(harmonic(k)))
          yield built :+ Product(_Number(ak), Cos(harmonic(k)))
                       :+ Product(_Number(bk), Sin(harmonic(k)))
        }
      }
      terms.map(ts => simplifyFully(ts.reduceLeft(Sum.apply)))
    }

/** The Maclaurin coefficients `c_k = f⁽ᵏ⁾(0)/k!` for `k = 0 .. upTo`, as plain numbers.
 *
 *  Every coefficient must fold to a finite number in `env`; a symbolic one, or a
 *  derivative outside `derive`'s rule table, aborts the whole vector.  Used by
 *  [[padeApproximant]], which needs the coefficients numerically to solve a linear system.
 */
private def maclaurinCoefficients(
    e: _Expression, v: _Variable, upTo: Int, env: Environment
): Option[Vector[Double]] =
  (0 to upTo).foldLeft(Option(Vector.empty[Double])) { (acc, k) =>
    acc.flatMap { built =>
      val dk = deriveN(e, v, k)
      if hasDerivative(dk) then None
      else
        val atZero = substitute(dk, Map(v.variable -> _Number(0)))
        (atZero.eval(env), factorialOf(k.toDouble)) match
          case (Right(_Number(d)), Some(f)) if !d.isNaN && !d.isInfinite => Some(built :+ d / f)
          case _                                                         => None
    }
  }


/** The Padé approximant `[m/n]` of `e` about zero: the rational function `P/Q` with
 *  `deg P ≤ m`, `deg Q ≤ n` and `Q(0) = 1` whose Maclaurin series agrees with `e`'s
 *  through order `m + n`.
 *
 *  Often a far better approximation than the Taylor polynomial of the same total degree,
 *  because a rational function can model a nearby pole that no polynomial can.
 *
 *  Construction: normalising `q₀ = 1`, matching the series through order `m + n` gives the
 *  `n × n` linear system `Σ(j = 1..n) q_j·c_{m+k-j} = −c_{m+k}` for `k = 1..n` (with
 *  `c_i = 0` for `i < 0`), after which `p_k = c_k + Σ(j = 1..min(k,n)) q_j·c_{k-j}`.
 *
 *  The system is solved with `core._MatrixValue.inverse`, **not** `equation.solveSystem`:
 *  `equation` imports `scalar`, so reaching the other way would be a dependency cycle.
 *  `core` is the common layer both can use.
 *
 *  @param e   the expression to approximate
 *  @param v   the variable; free in the result
 *  @param m   the numerator degree
 *  @param n   the denominator degree
 *  @param env environment in which the coefficients must fold to numbers
 *  @return the rational approximant, or `None` when a degree is out of range, a
 *          coefficient will not reduce, or the system is singular (no `[m/n]` approximant
 *          exists in normal form)
 */
def padeApproximant(
    e: _Expression, v: _Variable, m: Int, n: Int, env: Environment
): Option[_Expression] =
  if m < 0 || n < 0 || m + n > MaxTaylorOrder then None
  else
    maclaurinCoefficients(e, v, m + n, env).flatMap { coeffs =>
      def c(i: Int): Double = if i < 0 then 0.0 else coeffs(i)

      // Denominator coefficients q_1..q_n; with n = 0 there is no system and Q = 1.
      val denominator: Option[Vector[Double]] =
        if n == 0 then Some(Vector.empty)
        else
          val a   = Array.tabulate(n * n)(idx => c(m + (idx / n + 1) - (idx % n + 1)))
          val rhs = Array.tabulate(n)(k => -c(m + k + 1))
          _MatrixValue(n, n, a).inverse
            .map(inv => inv.multiply(_MatrixValue(n, 1, rhs)).toVector)
            .filter(_.forall(d => !d.isNaN && !d.isInfinite))

      denominator.map { q =>
        // p_k = c_k + sum over j of q_j * c_{k-j}
        val p = (0 to m).map(k => c(k) + (1 to math.min(k, n)).map(j => q(j - 1) * c(k - j)).sum)
        def poly(cs: Seq[Double], constant: Double): _Expression =
          cs.zipWithIndex.foldLeft(_Number(constant): _Expression) { case (acc, (coef, i)) =>
            Sum(acc, Product(_Number(coef), Power(v, _Number(i + 1))))
          }
        val numerator   = poly(p.drop(1), p.head)
        val denominator = poly(q, 1.0)
        simplifyFully(Ratio(numerator, denominator))
      }
    }