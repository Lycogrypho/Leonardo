package it.grypho.scala.leonardo
package scalar

import core.*


/** Polynomial normalization in a single variable.
 *
 *  This is the like-term collection that [[simplify]] deliberately does not attempt
 *  (see `Simplify.scala`).  It is the prerequisite of the equation solver: a linear
 *  equation is solvable exactly when its lhs - rhs collects to a degree-1 coefficient
 *  vector.
 *
 *  [[collect]] extracts the dense coefficient list of `e` as a polynomial in `v`,
 *  returning `Some(Vector(c0, c1, ..., cn))` with `e = c0 + c1*v + ... + cn*v^n`, each
 *  `ci` free of `v` and simplified.  Sums zip-add coefficient vectors, products
 *  convolve them, constant denominators divide through, and positive literal integer
 *  powers (capped at 20 like [[expand]]) are expanded by repeated convolution.
 *  Forms that are not polynomial in `v` -- `sin(v)`, `1/v`, `2^v`, a non-integer
 *  power -- yield `None`.
 *
 *  Coefficients are normalized in `v` only: a coefficient like `(a + 2a)` stays as
 *  [[simplify]] leaves it; recursive multivariate normal form is out of scope.
 *
 *  [[normalize]] rebuilds the collected polynomial as `c0 + c1*v + c2*v^2 + ...`
 *  (zero terms dropped, unit coefficients elided), so `10x - 2x` becomes `8x`
 *  regardless of tree shape.  Non-polynomial expressions are returned unchanged,
 *  and `_ElementWise` containers (matrices, equations) normalize per element/side.
 */
private val MaxPowerExpansion = 20

/** Extracts the dense coefficient vector of `e` as a polynomial in `v`.
 *
 *  Returns `Some(Vector(c0, c1, ..., cn))` where `e = c0 + c1*v + ... + cn*v^n` and
 *  every `ci` is free of `v`.  Returns `None` when `e` is not polynomial in `v`
 *  (e.g. `sin(v)`, `1/v`, `2^v`, a fractional power).
 *
 *  @param e the expression to analyse
 *  @param v the polynomial variable
 *  @return `Some` coefficient vector (length = degree + 1) or `None` for non-polynomial shapes
 */
def collect(e: _Expression, v: _Variable): Option[Vector[_Expression]] =
  val zero: _Expression = _Number(0)

  def add(a: Vector[_Expression], b: Vector[_Expression]): Vector[_Expression] =
    a.zipAll(b, zero, zero).map((p, q) => Sum(p, q))

  def mul(a: Vector[_Expression], b: Vector[_Expression]): Vector[_Expression] =
    val out = Array.fill[_Expression](a.size + b.size - 1)(zero)
    for i <- a.indices; j <- b.indices do
      out(i + j) = Sum(out(i + j), Product(a(i), b(j)))
    out.toVector

  def loop(e: _Expression): Option[Vector[_Expression]] = e match
    case _ if !dependsOn(e, v)                    => Some(Vector(e))
    case x: _Variable if x.variable == v.variable => Some(Vector(zero, _Number(1)))
    case Sum(a, b)                                => for ca <- loop(a); cb <- loop(b) yield add(ca, cb)
    case Product(a, b)                            => for ca <- loop(a); cb <- loop(b) yield mul(ca, cb)
    case Ratio(a, b) if !dependsOn(b, v)          => loop(a).map(_.map(c => Ratio(c, b)))
    case Power(_, _Number(0.0))                   => Some(Vector(_Number(1)))
    case Power(a, _Number(n)) if n >= 1 && n == n.toInt && n <= MaxPowerExpansion =>
      loop(a).map(ca => (1 until n.toInt).foldLeft(ca)((acc, _) => mul(acc, ca)))
    case _                                        => None   // sin(v), 1/v, 2^v, v^2.5, ...

  loop(e).map(cs => trimTrailingZeros(cs.map(simplifyFully)))

/** Drops trailing zero coefficients; always returns at least `Vector(_Number(0))`. */
private def trimTrailingZeros(cs: Vector[_Expression]): Vector[_Expression] =
  val trimmed = cs.reverse.dropWhile(_ == _Number(0)).reverse
  if trimmed.isEmpty then Vector(_Number(0)) else trimmed

/** Rebuilds `e` as a sum of like terms collected as a polynomial in `v`.
 *
 *  Non-polynomial expressions (where [[collect]] returns `None`) are returned unchanged.
 *  `_ElementWise` containers (matrix literals, equations) are processed element-wise.
 *
 *  @param e the expression to normalise
 *  @param v the polynomial variable
 *  @return the equivalent expression with like terms folded (e.g. `10x - 2x` becomes `8x`)
 */
def normalize(e: _Expression, v: _Variable): _Expression = e match
  // element-wise containers (matrices, matrix sums, transpose, equations):
  // normalize each element / each side
  case ew: _ElementWise => ew.rebuild(ew.children.map(normalize(_, v)))
  case _ =>
    collect(e, v) match
      case None => e   // not polynomial in v: unchanged
      case Some(cs) =>
        val terms = cs.zipWithIndex.flatMap { (c, k) =>
          if c == _Number(0) then None
          else Some(k match
            case 0 => c
            case 1 => if c == _Number(1) then v else Product(c, v)
            case _ => if c == _Number(1) then Power(v, _Number(k))
                      else Product(c, Power(v, _Number(k))))
        }
        if terms.isEmpty then _Number(0) else terms.reduceLeft(Sum(_, _))
