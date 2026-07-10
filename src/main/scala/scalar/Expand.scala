package it.grypho.scala.leonardo
package scalar

import core.*


/**
 * Expand an expression by distributing multiplication over addition and
 * expanding (a+b)^n for positive integer exponents (capped at 20 to avoid
 * combinatorial blowup).
 *
 * Like terms are NOT combined; call simplify() afterwards to fold constants
 * and remove identity elements.
 */
def expand(e: _Expression): _Expression = e match
  case _: _Number   => e
  case _: _Variable => e

  // Recurse into sums without distributing (distribution happens at Product).
  case Sum(a, b) => Sum(expand(a), expand(b))

  // Distribute: (x + y) * z  and  x * (y + z).
  // Both sides are expanded first so nested sums surface before we distribute.
  case Product(a, b) =>
    (expand(a), expand(b)) match
      case (Sum(x, y), z) => expand(Sum(Product(x, z), Product(y, z)))
      case (x, Sum(y, z)) => expand(Sum(Product(x, y), Product(x, z)))
      case (x, y)         => Product(x, y)

  case Ratio(a, b) => Ratio(expand(a), expand(b))

  // (sum)^n  via repeated multiplication — (sum)^k * (sum) at each step.
  case Power(base, _Number(n)) =>
    val ni = n.toLong
    val eb = expand(base)
    eb match
      case Sum(_, _) if ni >= 1 && ni.toDouble == n && ni <= 20 =>
        (1L until ni).foldLeft(eb)((acc, _) => expand(Product(eb, acc)))
      case _ => Power(eb, _Number(n))

  case Power(a, b) => Power(expand(a), expand(b))

  case Exp(a)   => Exp(expand(a))
  case Log(a)   => Log(expand(a))
  case Sin(a)   => Sin(expand(a))
  case Cos(a)   => Cos(expand(a))
  case Tg(a)    => Tg(expand(a))
  case Asin(a)  => Asin(expand(a))
  case Acos(a)  => Acos(expand(a))
  case Atan(a)  => Atan(expand(a))

  case _Derivative(f, v)          => _Derivative(expand(f), v)
  case _Integral(f, v)            => _Integral(expand(f), v)
  case _DefIntegral(f, v, lo, hi) => _DefIntegral(expand(f), v, expand(lo), expand(hi))
  // Element-wise containers (see core._ElementWise): expand each child in place.
  case ew: _ElementWise           => ew.rebuild(ew.children.map(expand))
  case other                      => other
