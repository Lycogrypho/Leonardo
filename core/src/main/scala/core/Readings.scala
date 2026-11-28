package it.grypho.scala.leonardo
package core

/** The grid point an exact value is a working-precision reading of, or `None` (F_0384, the owner's decision: the
 *  integer).
 *
 *  In the exact tier every irrational is carried as a rational at the working precision, so a product or quotient of
 *  readings lands `~10⁻ʷᵖ` beside the value it stands for: `log(10^15)` is `15 − 10⁻³⁰`, `(3^(1/3))^3` likewise beside
 *  `3`.  `==` already calls such a value the integer (F_0177); an integer part read it exactly and took the other
 *  side (`floor(log(10^15))` was `14`).  A grid point `g = k + offset` (integers for `floor`/`ceil`, half-integers for
 *  `round`) is the value's when `v` is not on the grid, lies within `10^(−wp/2)` of `g`, and reading `e` again at
 *  twice the working precision brings it closer to `g` by about the digits gained (F_0376's test): a reading of
 *  `g` converges to it, a true exact value does not move, and a point merely beside `g` that holds an irrational
 *  moves but keeps its distance.
 *
 *  @param e      the expression `v` is the value of, read again at the finer precision
 *  @param v      its value at `env`'s working precision
 *  @param offset the grid's offset: `0` for the integers, `1/2` for the half-integers
 *  @param env    the bindings and the working precision
 *  @return the grid point, exact, or `None` when `v` is no reading of one (or is on the grid exactly)
 */
private[leonardo] def gridPointRead(e: _Expression, v: _Value, offset: _Rational, env: Environment): Option[_Rational] =
  def distance(x: _Value, g: _Rational): Option[Double] = _Surd.subtract(x, g).collect { case d @ _Number(dd) if _Surd.isExact(d) => math.abs(dd) }
  val gained = math.pow(10.0, -env.workingPrecision / 2.0)
  v match
    case _Number(d) if _Surd.isExact(v) && d.isFinite =>
      for
        k     <- _Rational.fromDouble(math.rint(d - offset.toDouble))
        g      = k.add(offset)
        now   <- distance(v, g)
        if now > 0.0 && now <= gained * math.max(1.0, math.abs(d))
        finer <- evalNumeric(e, env.withWorkingPrecision(2 * env.workingPrecision)).toOption.filter(_Surd.isExact)
        dist2 <- distance(finer, g)
        if dist2 <= now * gained
      yield g
    case _ => None

/** The integer an argument that must be whole stands for (F_0394): a `Double` that is whole; an exact integer; an exact
 *  working-precision reading of one ([[gridPointRead]], F_0384's decision); and `None` for a true exact non-integer,
 *  which a `Double` reading took as whole, so `binom(4, 2 + 1/10^20)` was `6` and `eye(2 + 1/10^20)` the identity.
 *  @param e   the argument's expression, read again finely to tell a reading
 *  @param v   its value
 *  @param env the bindings and the working precision
 *  @return the integer as a `Double`, or `None` when `v` is not whole */
private[leonardo] def wholeValue(e: _Expression, v: _Value, env: Environment): Option[Double] = v match
  case q: _Rational => q.toBigIntExact.map(_.toDouble).orElse(gridPointRead(e, q, _Rational.Zero, env).map(_.toDouble))
  case s if _Surd.isExact(s) => gridPointRead(e, s, _Rational.Zero, env).map(_.toDouble)
  case _Number(d) => Option.when(Whole.isWhole(d))(d)
  case _ => None

/** [[gridPointRead]] of `e`'s value at `env`, when that value is exact.
 *
 *  The value is [[evalNumeric]]'s, so a named constant is read at the working precision: in exact mode `pi` stays a
 *  symbol, and `atan(1)*4/pi` is a reading of `1` only once `pi` is read too (each reading refines with the
 *  precision, which is what the convergence test needs).
 *
 *  @param e      the expression
 *  @param offset the grid's offset
 *  @param env    the bindings and the working precision
 *  @return the grid point `e` is a reading of, or `None` */
private[leonardo] def gridPointOf(e: _Expression, offset: _Rational, env: Environment): Option[_Rational] =
  evalNumeric(e, env).toOption.flatMap(v => gridPointRead(e, v, offset, env))
