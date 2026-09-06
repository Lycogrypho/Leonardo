package it.grypho.scala.leonardo
package scalar

import core.*
import scala.math.pow


/** Single-pass structural simplification applied bottom-up.
 *
 *  Rules applied:
 *   - Identity / absorbing elements  (`0+x`, `1*x`, `0*x`, `x/1`, `x^0`, `x^1`, `1^x`)
 *   - Constant folding               (`_Number op _Number → _Number`)
 *   - Same-operand rules             (`x+x → 2x`, `x*x → x^2`, `x/x → 1`)
 *   - Double negation                (`(-1)*(-1*x) → x`)
 *   - Inverse function pairs         (`log(exp(x)) → x`, `exp(log(x)) → x`)
 *   - Known function values at 0     (`sin(0)→0`, `cos(0)→1`, `tg(0)→0`, `exp(0)→1`, `log(1)→0`)
 *
 *  This is NOT a full normaliser: like terms in different sub-trees (`x + (x + y)`)
 *  are not combined.  For that, call [[simplifyFully]] which iterates until fixpoint.
 *  Results are memoised; [[simplifyFully]]'s fixpoint passes and shared sub-trees hit
 *  the cache instead of re-walking the rule table.
 *
 *  @param e the expression to simplify
 *  @return a structurally simplified equivalent expression
 */
// Memoized entry point: simplify is pure in e, so results are cached across calls.
// simplifyFully's fixpoint iteration and shared subtrees hit the cache instead of
// re-walking the rule table (the pragmatic core of the legacy hash-consing idea).
private val simplifyMemo = new Memo[_Expression, _Expression](10000)

def simplify(e: _Expression): _Expression =
  simplifyMemo.getOrElseUpdate(e)(simplifyImpl(e))

/** The environment constant folding runs in — empty, because `simplify` ignores bindings
 *  by design.  Its working precision is the default, which only matters for a fractional
 *  power of an exact base.
 */
private val foldEnv = new Environment()

/** Folds a node whose operands are already concrete, by *evaluating* it.
 *
 *  Delegating to `eval` rather than re-implementing `+`/`*`/`/`/`^` here is what keeps
 *  `simplify` and `eval` from disagreeing — which stopped being cosmetic once "concrete"
 *  covered both `_Number` and the exact `_Rational` (issue 4.L).  Re-implemented folding
 *  produced `0.66667` for `simplify 1/3 + 1/3` while `eval` gave `1/2` on the same input.
 *
 *  A node that does not reduce (division by zero, a non-finite power) comes back unchanged,
 *  which is exactly what the hand-written guards used to do.
 *
 *  @param node the operation to fold, with both operands concrete
 *  @return the folded value, or `node` when it does not reduce
 */
private def fold(node: _Expression): _Expression =
  node.eval(foldEnv) match
    case Right(v) => v
    case Left(_)  => node

private def simplifyImpl(e: _Expression): _Expression = e match
  case _: _Number   => e
  case _: _Complex  => e
  case _: _Variable => e

  case Sum(a, b) =>
    (simplify(a), simplify(b)) match
      case (_Number(d), y) if d == 0.0 => y
      case (x, _Number(d)) if d == 0.0 => x
      case (x @ _Number(_), y @ _Number(_)) => fold(Sum(x, y))
      case (x, y) if x == y            => simplify(Product(_Number(2), x))
      case (x, Product(_Number(d), y)) if d == -1.0 && x == y => _Number(0)
      case (Product(_Number(d), x), y) if d == -1.0 && x == y => _Number(0)
      // Constant folding in nested sums: (x + c1) + c2 → x + (c1+c2)
      // The constants are bound whole and re-folded through `fold`, rather than taken as
      // their Doubles, so merging two exact coefficients does not produce an inexact one.
      case (Sum(x, c1 @ _Number(_)), c2 @ _Number(_)) => simplify(Sum(x, fold(Sum(c1, c2))))
      case (Sum(c1 @ _Number(_), x), c2 @ _Number(_)) => simplify(Sum(x, fold(Sum(c1, c2))))
      case (c1 @ _Number(_), Sum(x, c2 @ _Number(_))) => simplify(Sum(x, fold(Sum(c1, c2))))
      case (c1 @ _Number(_), Sum(c2 @ _Number(_), x)) => simplify(Sum(x, fold(Sum(c1, c2))))
      case (x, y)                      => Sum(x, y)

  case Product(a, b) =>
    (simplify(a), simplify(b)) match
      case (_Number(d), _) if d == 0.0              => _Number(0)
      case (_, _Number(d)) if d == 0.0              => _Number(0)
      case (_Number(d), y) if d == 1.0              => y
      case (x, _Number(d)) if d == 1.0              => x
      case (x @ _Number(_), y @ _Number(_))         => fold(Product(x, y))
      // double negation, all four shapes: -1 * (-1 * x) → x and mirrors
      case (_Number(da), Product(_Number(db), x))
        if da == -1.0 && db == -1.0                 => x
      case (_Number(da), Product(x, _Number(db)))
        if da == -1.0 && db == -1.0                 => x
      case (Product(_Number(da), x), _Number(db))
        if da == -1.0 && db == -1.0                 => x
      case (Product(x, _Number(da)), _Number(db))
        if da == -1.0 && db == -1.0                 => x
      // Constant folding in nested products: (c1 * x) * c2 → (c1·c2) * x and mirrors
      case (Product(c1 @ _Number(_), x), c2 @ _Number(_)) => simplify(Product(fold(Product(c1, c2)), x))
      case (Product(x, c1 @ _Number(_)), c2 @ _Number(_)) => simplify(Product(fold(Product(c1, c2)), x))
      case (c1 @ _Number(_), Product(c2 @ _Number(_), x)) => simplify(Product(fold(Product(c1, c2)), x))
      case (c1 @ _Number(_), Product(x, c2 @ _Number(_))) => simplify(Product(fold(Product(c1, c2)), x))
      case (x, y) if x == y                         => simplify(Power(x, _Number(2)))
      case (x, y)                                   => Product(x, y)

  case Ratio(a, b) =>
    (simplify(a), simplify(b)) match
      case (_Number(d), y) if d == 0.0 && y != _Number(0) => _Number(0)
      case (x, _Number(d)) if d == 1.0  => x
      // Reuses the matched -1 rather than building a fresh one, so an exact -1 stays exact.
      case (x, c @ _Number(d)) if d == -1.0 => simplify(Product(c, x))
      case (x @ _Number(_), y @ _Number(_)) => fold(Ratio(x, y))
      case (x, y) if x == y && x != _Number(0) => _Number(1)
      // Reciprocal/quotient normalisation into the reciprocal functions (issue 3.9), so the
      // library has exactly one spelling of each: 1/cos → sec, cos/sin → cot, and so on.
      case (_Number(d), Cos(u))  if d == 1.0 => Sec(u)
      case (_Number(d), Sin(u))  if d == 1.0 => Csc(u)
      case (_Number(d), Tg(u))   if d == 1.0 => Cot(u)
      case (_Number(d), Cosh(u)) if d == 1.0 => Sech(u)
      case (_Number(d), Sinh(u)) if d == 1.0 => Csch(u)
      case (_Number(d), Tanh(u)) if d == 1.0 => Coth(u)
      case (_Number(d), Power(Cos(u), n))  if d == 1.0 => Power(Sec(u), n)
      case (_Number(d), Power(Sin(u), n))  if d == 1.0 => Power(Csc(u), n)
      case (_Number(d), Power(Cosh(u), n)) if d == 1.0 => Power(Sech(u), n)
      case (_Number(d), Power(Sinh(u), n)) if d == 1.0 => Power(Csch(u), n)
      case (Cos(u), Sin(w))   if u == w => Cot(u)
      case (Cosh(u), Sinh(w)) if u == w => Coth(u)
      case (x, y)                      => Ratio(x, y)

  case Power(a, b) =>
    (simplify(a), simplify(b)) match
      // x^0 = 1, including 0^0 = 1 — the common CAS/IEEE convention, and consistent with
      // Power.eval (Java pow(0, 0) = 1.0). Both places must agree so simplify then eval
      // (and vice versa) never disagree on 0^0.
      case (x, _Number(d)) if d == 0.0 => _Number(1)
      case (x, _Number(d)) if d == 1.0 => x
      case (_Number(d), _) if d == 1.0 => _Number(1)
      case (x @ _Number(_), y @ _Number(_)) => fold(Power(x, y))
      case (x, y)                      => Power(x, y)

  case Exp(a) =>
    simplify(a) match
      case _Number(d) if d == 0.0 => _Number(1)
      case Ln(x)                  => x             // exp(ln(x)) = x
      case x                      => Exp(x)

  case Ln(a) =>
    simplify(a) match
      case _Number(d) if d == 1.0 => _Number(0)
      case Exp(x)                 => x             // ln(exp(x)) = x
      case x                      => Ln(x)

  case LogBase(a, b) =>
    (simplify(a), simplify(b)) match
      case (_Number(d), _) if d == 1.0 => _Number(0)   // log_b(1) = 0
      case (x, y) if x == y            => _Number(1)   // log_b(b) = 1
      case (x, y)                      => LogBase(x, y)

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
      case _Number(d) if d == 0.0  => _Number(0)
      case _Number(d) if d == 1.0  => _Number(math.Pi / 2)
      case _Number(d) if d == -1.0 => _Number(-math.Pi / 2)
      case x                       => Asin(x)

  case Acos(a) =>
    simplify(a) match
      case _Number(d) if d == 1.0  => _Number(0)
      case _Number(d) if d == 0.0  => _Number(math.Pi / 2)
      case _Number(d) if d == -1.0 => _Number(math.Pi)
      case x                       => Acos(x)

  case Atan(a) =>
    simplify(a) match
      case _Number(d) if d == 0.0 => _Number(0)
      case _Number(d) if d == 1.0 => _Number(math.Pi / 4)
      case x                      => Atan(x)

  // hyperbolic and reciprocal-trigonometric functions (issue 3.9): inverse-function pairs
  // (only the always-valid direction — acosh(cosh(x)) = |x|, so it is deliberately absent)
  // and known values at zero.
  case Sinh(a) =>
    simplify(a) match
      case _Number(d) if d == 0.0 => _Number(0)
      case Asinh(x)               => x
      case x                      => Sinh(x)

  case Cosh(a) =>
    simplify(a) match
      case _Number(d) if d == 0.0 => _Number(1)
      case Acosh(x)               => x             // cosh(acosh(x)) = x on acosh's domain x ≥ 1
      case x                      => Cosh(x)

  case Tanh(a) =>
    simplify(a) match
      case _Number(d) if d == 0.0 => _Number(0)
      case Atanh(x)               => x
      case x                      => Tanh(x)

  case Asinh(a) =>
    simplify(a) match
      case _Number(d) if d == 0.0 => _Number(0)
      case Sinh(x)                => x
      case x                      => Asinh(x)

  case Acosh(a) =>
    simplify(a) match
      case _Number(d) if d == 1.0 => _Number(0)
      case x                      => Acosh(x)

  case Atanh(a) =>
    simplify(a) match
      case _Number(d) if d == 0.0 => _Number(0)
      case Tanh(x)                => x
      case x                      => Atanh(x)

  // sec(0) = 1/cos(0) = 1 and sech(0) = 1/cosh(0) = 1; the remaining reciprocals
  // (csc/cot/csch/coth) have a pole at 0, so they stay symbolic there.
  case Sec(a) =>
    simplify(a) match
      case _Number(d) if d == 0.0 => _Number(1)
      case x                      => Sec(x)
  case Csc(a)  => Csc(simplify(a))
  case Cot(a)  => Cot(simplify(a))
  case Sech(a) =>
    simplify(a) match
      case _Number(d) if d == 0.0 => _Number(1)
      case x                      => Sech(x)
  case Csch(a) => Csch(simplify(a))
  case Coth(a) => Coth(simplify(a))

  case _Heaviside(a) =>
    simplify(a) match
      case _Number(d) => _Number(if d >= 0 then 1.0 else 0.0)
      case sa         => _Heaviside(sa)
  case _Derivative(f, v)          => _Derivative(simplify(f), v)
  case _Integral(f, v)            => _Integral(simplify(f), v)
  case _DefIntegral(f, v, lo, hi) => _DefIntegral(simplify(f), v, simplify(lo), simplify(hi))
  case _Limit(f, bv, pt, dir)     => _Limit(simplify(f), bv, simplify(pt), dir)
  // Element-wise containers (see core._ElementWise): simplify each child in place.
  case ew: _ElementWise           => ew.rebuild(ew.children.map(simplify))
  case other                      => other


/** Iterates [[simplify]] until the result stops changing (fixpoint).
 *
 *  Terminates because [[simplify]] never increases expression size.
 *
 *  @param e the expression to simplify fully
 *  @return the fully simplified expression
 */
@annotation.tailrec
def simplifyFully(e: _Expression): _Expression =
  val s = simplify(e)
  if s == e then e else simplifyFully(s)
