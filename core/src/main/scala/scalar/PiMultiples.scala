package it.grypho.scala.leonardo
package scalar

import core.*


// Exact trigonometric values at rational multiples of pi (F_0289).
//
// With `pi` a symbol in exact mode, `sin(pi/6)` can be answered as `1/2` rather than read at the
// working precision.  The angles are counted in twelfths of pi: the table holds the multiples of
// pi/6 and pi/4, whose values are rationals or a single surd; the odd twelfths need two radicals
// (sin(pi/12) = (sqrt 6 - sqrt 2)/4), which `_Surd` cannot hold, and are read at the working precision.

/** `e` as `q*pi` with `q` exact, if it is one: a product or quotient of `pi` by exact rationals,
 *  or a sum of such. */
private[scalar] def piMultiple(e: _Expression): Option[_Rational] = e match
  case _Constant(ConstantKind.Pi, _) => Some(_Rational(1))
  case Product(a, b)                 => exactly(a).flatMap(x => piMultiple(b).map(x.multiply(_)))
                                          .orElse(exactly(b).flatMap(y => piMultiple(a).map(_.multiply(y))))
  case Ratio(a, b)                   => exactly(b).flatMap(y => piMultiple(a).flatMap(_.divide(y)))
  case Sum(a, b)                     => for x <- piMultiple(a); y <- piMultiple(b) yield x.add(y)
  case _                             => None

/** A numeric literal as the rational it is: a `Double` is read as the dyadic it holds, so `2.0`
 *  is `2` and a `1/3` that became a `Double` is not a third (and so never a table angle). */
private def exactly(e: _Expression): Option[_Rational] = e match
  case r: _Rational => Some(r)
  case n: _Number   => _Rational.fromDouble(n.d)
  case _            => None

/** The angle `e` in twelfths of pi, reduced to `[0, 24)`, when it is a whole number of them. */
private[scalar] def piTwelfths(e: _Expression): Option[Int] =
  piMultiple(e).flatMap { q =>
    val twelve = q.multiply(_Rational(12))
    Option.when(twelve.num.mod(twelve.den.abs) == 0)((twelve.num / twelve.den).mod(BigInt(24)).toInt)
  }

/** `n/d` exact. */
private def frac(n: Int, d: Int): _Value = _Rational.of(BigInt(n), BigInt(d)).getOrElse(_Number(n.toDouble / d))

/** `c*sqrt(r)` exact, `c = n/d`. */
private def surd(n: Int, d: Int, r: Int): _Value =
  _Rational.of(BigInt(n), BigInt(d)).flatMap(c => _Surd.of(_Rational.Zero, c, BigInt(r)))
    .getOrElse(_Number(n * math.sqrt(r) / d))

/** sin at `t` twelfths of pi, for the angles whose sine is a rational or one surd. */
private[scalar] def sinAtTwelfths(t: Int): Option[_Value] =
  val u = Math.floorMod(t, 24)
  val first: Option[_Value] = u % 12 match
    case 0     => Some(frac(0, 1))
    case 2 | 10 => Some(frac(1, 2))
    case 3 | 9  => Some(surd(1, 2, 2))
    case 4 | 8  => Some(surd(1, 2, 3))
    case 6     => Some(frac(1, 1))
    case _     => None
  if u < 12 then first else first.map(negated)

/** tan at `t` twelfths of pi, away from its poles. */
private[scalar] def tanAtTwelfths(t: Int): Option[_Value] =
  Math.floorMod(t, 12) match
    case 0  => Some(frac(0, 1))
    case 2  => Some(surd(1, 3, 3))
    case 3  => Some(frac(1, 1))
    case 4  => Some(surd(1, 1, 3))
    case 8  => Some(surd(-1, 1, 3))
    case 9  => Some(frac(-1, 1))
    case 10 => Some(surd(-1, 3, 3))
    case _  => None

/** `-v` for an exact value. */
private def negated(v: _Value): _Value = v match
  case r: _Rational => r.multiply(_Rational(-1))
  case s: _Surd     => _Surd.negate(s).getOrElse(s)
  case other        => other
