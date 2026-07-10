package it.grypho.scala.leonardo
package scalar

import core.*
import scala.math.pow


/**
 * Single-pass structural simplification. Applies algebraic identities bottom-up:
 *   - Identity / absorbing elements  (0+x, 1*x, 0*x, x/1, x^0, x^1, 1^x)
 *   - Constant folding               (_Number op _Number → _Number)
 *   - Same-operand rules             (x+x → 2x, x*x → x^2, x/x → 1)
 *   - Double negation                (-1)*(-1*x) → x
 *   - Inverse function pairs         (log(exp(x)) → x, exp(log(x)) → x)
 *   - Known function values at 0     (sin(0)→0, cos(0)→1, tg(0)→0, exp(0)→1, log(1)→0)
 *
 * This is NOT a full normaliser: like terms in different sub-trees (x + (x + y))
 * are not combined. For that, call simplify() iteratively until the result stabilises.
 */
// Memoized entry point: simplify is pure in e, so results are cached across calls.
// simplifyFully's fixpoint iteration and shared subtrees hit the cache instead of
// re-walking the rule table (the pragmatic core of the legacy hash-consing idea).
private val simplifyMemo = new Memo[_Expression, _Expression](10000)

def simplify(e: _Expression): _Expression =
  simplifyMemo.getOrElseUpdate(e)(simplifyImpl(e))

private def simplifyImpl(e: _Expression): _Expression = e match
  case _: _Number   => e
  case _: _Variable => e

  case Sum(a, b) =>
    (simplify(a), simplify(b)) match
      case (_Number(d), y) if d == 0.0 => y
      case (x, _Number(d)) if d == 0.0 => x
      case (_Number(da), _Number(db))  => _Number(da + db)
      case (x, y) if x == y            => simplify(Product(_Number(2), x))
      case (x, Product(_Number(d), y)) if d == -1.0 && x == y => _Number(0)
      case (Product(_Number(d), x), y) if d == -1.0 && x == y => _Number(0)
      case (x, y)                      => Sum(x, y)

  case Product(a, b) =>
    (simplify(a), simplify(b)) match
      case (_Number(d), _) if d == 0.0              => _Number(0)
      case (_, _Number(d)) if d == 0.0              => _Number(0)
      case (_Number(d), y) if d == 1.0              => y
      case (x, _Number(d)) if d == 1.0              => x
      case (_Number(da), _Number(db))               => _Number(da * db)
      // double negation, all four shapes: -1 * (-1 * x) → x and mirrors
      case (_Number(da), Product(_Number(db), x))
        if da == -1.0 && db == -1.0                 => x
      case (_Number(da), Product(x, _Number(db)))
        if da == -1.0 && db == -1.0                 => x
      case (Product(_Number(da), x), _Number(db))
        if da == -1.0 && db == -1.0                 => x
      case (Product(x, _Number(da)), _Number(db))
        if da == -1.0 && db == -1.0                 => x
      case (x, y) if x == y                         => simplify(Power(x, _Number(2)))
      case (x, y)                                   => Product(x, y)

  case Ratio(a, b) =>
    (simplify(a), simplify(b)) match
      case (_Number(d), y) if d == 0.0 && y != _Number(0) => _Number(0)
      case (x, _Number(d)) if d == 1.0 => x
      case (_Number(da), _Number(db)) if db != 0.0 => _Number(da / db)
      case (x, y) if x == y && x != _Number(0) => _Number(1)
      case (x, y)                      => Ratio(x, y)

  case Power(a, b) =>
    (simplify(a), simplify(b)) match
      case (x, _Number(d)) if d == 0.0 && x != _Number(0) => _Number(1)
      case (x, _Number(d)) if d == 1.0 => x
      case (_Number(d), _) if d == 1.0 => _Number(1)
      case (_Number(da), _Number(db)) if !(da == 0.0 && db == 0.0) => _Number(pow(da, db))
      case (x, y)                      => Power(x, y)

  case Exp(a) =>
    simplify(a) match
      case _Number(d) if d == 0.0 => _Number(1)
      case Log(x)                 => x             // exp(log(x)) = x
      case x                      => Exp(x)

  case Log(a) =>
    simplify(a) match
      case _Number(d) if d == 1.0 => _Number(0)
      case Exp(x)                 => x             // log(exp(x)) = x
      case x                      => Log(x)

  case Sin(a) =>
    simplify(a) match
      case _Number(d) if d == 0.0 => _Number(0)
      case x                      => Sin(x)

  case Cos(a) =>
    simplify(a) match
      case _Number(d) if d == 0.0 => _Number(1)
      case x                      => Cos(x)

  case Tg(a) =>
    simplify(a) match
      case _Number(d) if d == 0.0 => _Number(0)
      case x                      => Tg(x)

  case Asin(a) =>
    simplify(a) match
      case _Number(d) if d == 0.0 => _Number(0)
      case x                      => Asin(x)

  case Acos(a) =>
    simplify(a) match
      case _Number(d) if d == 1.0 => _Number(0)
      case x                      => Acos(x)

  case Atan(a) =>
    simplify(a) match
      case _Number(d) if d == 0.0 => _Number(0)
      case x                      => Atan(x)

  case _Derivative(f, v)          => _Derivative(simplify(f), v)
  case _Integral(f, v)            => _Integral(simplify(f), v)
  case _DefIntegral(f, v, lo, hi) => _DefIntegral(simplify(f), v, simplify(lo), simplify(hi))
  // Element-wise containers (see core._ElementWise): simplify each child in place.
  case ew: _ElementWise           => ew.rebuild(ew.children.map(simplify))
  case other                      => other


// Iterate simplify until the result stops changing (fixpoint). Terminates because
// simplify never increases expression size.
@annotation.tailrec
def simplifyFully(e: _Expression): _Expression =
  val s = simplify(e)
  if s == e then e else simplifyFully(s)
