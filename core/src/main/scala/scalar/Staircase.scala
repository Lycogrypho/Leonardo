package it.grypho.scala.leonardo
package scalar

import core.*


/** The definite integral of a staircase (F_0340): an integrand holding `floor`, `ceil`, `round` or `mod`
 *  of a **linear** argument, split at the jumps and integrated piece by piece.
 *
 *  No antiderivative closes over a jump, and adaptive Simpson does not settle across one, so
 *  `integral(floor(x), x, 0, 3)` stayed unevaluated.  Between two consecutive jumps every integer part
 *  is constant — `floor`, `ceil` and `round` take the value they have at the piece's midpoint, and
 *  `mod(p, q)` is `p − q·⌊p/q⌋` with that floor fixed — so each piece is an ordinary definite integral
 *  of a smooth integrand, answered by the existing tiers, and the pieces add up.  **Exact on exact
 *  input**: the jump points `(k − β)/α` and the constants are computed in the tier of the bounds and
 *  the coefficients, so `integral(floor(x), x, 0, 3)` is the exact `3`.
 *
 *  Declines (`None`, and the caller's tiers then run as before) when an integer part's argument is not
 *  linear in `v` with numeric coefficients — the jumps of `floor(x²)` could be enumerated, those of
 *  `floor(sin(x))` not, and a partial list would answer for a different function — when a `mod`'s
 *  divisor depends on `v`, when the pieces exceed [[core.Limit.MaxStaircasePieces]], or when a piece
 *  declines.
 */
private[scalar] object Staircase:

  /** An integer part of the integrand: `node`, the linear argument `slope·v + icpt` whose integers (or
   *  half-integers, for `round`) are its jumps, and the expression it is on a piece given the argument's
   *  value at the midpoint. */
  private final case class Part(node: _Expression, slope: _Value, icpt: _Value, halfIntegers: Boolean,
                                onPiece: _Value => Option[_Expression])

  private def num(x: _Value): Double = x match
    case _Number(d) => d
    case _          => Double.NaN

  /** The integral of `e` over `[lo, hi]` by pieces, or `None` when it does not apply or declines.
   *  @param e   the integrand
   *  @param v   the integration variable
   *  @param lo  the lower limit, evaluated
   *  @param hi  the upper limit, evaluated
   *  @param env bindings, precision and the budgets
   *  @return the integral, or `None` */
  def integral(e: _Expression, v: _Variable, lo: _Value, hi: _Value, env: Environment): Option[_Value] =
    val found = nodes(e, v)
    val (a, b) = (num(lo), num(hi))
    if found.isEmpty || !a.isFinite || !b.isFinite || a == b then None
    else
      val (l, h) = if a < b then (lo, hi) else (hi, lo)
      // the tier of the whole computation: the integrand's, or the bounds' (a bare `x` has no exact
      // content, and `collect` builds its unit slope in its own tier, so it is read with an exact zero)
      val exact = _Rational.containsExactIn(e, env) || _Rational.containsExact(lo) || _Rational.containsExact(hi)
      for
        parts  <- sequence(found.map(part(_, v, exact, env)))
        jumps  <- jumpPoints(parts, l, h, env)
        cuts    = l +: jumps.sortBy(num).distinctBy(num) :+ h
        values <- sequence(cuts.sliding(2).toList.map(pq => piece(e, v, parts, pq(0), pq(1), env)))
        total  <- values.reduceLeftOption((x, y) => Sum(x, y).eval(env).toOption.getOrElse(_Number(Double.NaN)))
        if num(total).isFinite
      yield if a < b then total
            else Product(_Rational.literalLike(-1, total), total).eval(env).toOption.getOrElse(total)

  /** The integer-part nodes of `e` depending on `v`. */
  private def nodes(e: _Expression, v: _Variable): List[_Expression] =
    val here = e match
      case Floor(g) if dependsOn(g, v)  => List(e)
      case Ceil(g) if dependsOn(g, v)   => List(e)
      case Round(g) if dependsOn(g, v)  => List(e)
      case Mod(_, _) if dependsOn(e, v) => List(e)
      case _                            => Nil
    here ++ e.children.flatMap(nodes(_, v))

  /** A node's linear argument, or `None` when it is not `slope·v + icpt` with numeric coefficients (or a
   *  `mod`'s divisor depends on `v`, or an integer part sits inside another's argument). */
  private def part(node: _Expression, v: _Variable, exact: Boolean, env: Environment): Option[Part] =
    def linear(g: _Expression): Option[(_Value, _Value)] =
      if nodes(g, v).nonEmpty then None
      else collect(if exact then Sum(g, _Rational(0)) else g, v).filter(_.sizeIs == 2).flatMap { cs =>
        for
          s <- cs(1).eval(env).toOption if num(s).isFinite && num(s) != 0
          i <- cs(0).eval(env).toOption if num(i).isFinite
        yield (s, i)
      }
    def own(f: _Expression => _Expression)(g: _Value): Option[_Expression] = f(g).eval(env).toOption
    node match
      case Floor(g) => linear(g).map((s, i) => Part(node, s, i, false, own(Floor.apply)))
      case Ceil(g)  => linear(g).map((s, i) => Part(node, s, i, false, own(Ceil.apply)))
      case Round(g) => linear(g).map((s, i) => Part(node, s, i, true, own(Round.apply)))
      case Mod(p, q) if !dependsOn(q, v) =>
        linear(Ratio(p, q)).map((s, i) => Part(node, s, i, false,
          g => Floor(g).eval(env).toOption.map(c => Sum(p, Product(_Rational.literalLike(-1, c), Product(q, c))))))
      case _ => None

  /** Every jump strictly inside `(l, h)`, in the tier of the bounds and coefficients, or `None` past the
   *  piece budget. */
  private def jumpPoints(parts: List[Part], l: _Value, h: _Value, env: Environment): Option[Vector[_Value]] =
    val budget = env.limits(Limit.MaxStaircasePieces)
    def at(p: Part, x: _Value): Double = Sum(Product(p.slope, x), p.icpt).eval(env).toOption.fold(Double.NaN)(num)
    val ranges = parts.map { p =>
      val (ga, gb) = (at(p, l), at(p, h))
      val shift    = if p.halfIntegers then 0.5 else 0.0
      // the k whose jump value k + shift lies strictly inside the argument's range
      (p, math.floor(math.min(ga, gb) - shift) + 1, math.ceil(math.max(ga, gb) - shift) - 1)
    }
    val count = ranges.map((_, first, last) => math.max(0.0, last - first + 1)).sum
    if !count.isFinite || count + 1 > budget then None
    else sequence(ranges.flatMap { (p, first, last) =>
      val exact = _Rational.containsExactIn(Sum(p.slope, p.icpt), env) || _Rational.containsExact(l)
      def lit(n: BigInt): _Value = if exact then _Rational(n) else _Number(n.toDouble)
      (first.toLong to last.toLong).map { k =>
        // v = (k + shift − icpt)/slope; the half of `round` written in the same tier
        val jump = if p.halfIntegers then Sum(lit(BigInt(k)), Ratio(lit(1), lit(2))) else lit(BigInt(k))
        Ratio(Sum(jump, Product(lit(-1), p.icpt)), p.slope).eval(env).toOption
      }
    }).map(_.toVector)

  /** The integral over one piece `[p, q]`, every integer part replaced by what it is there. */
  private def piece(e: _Expression, v: _Variable, parts: List[Part], p: _Value, q: _Value,
                    env: Environment): Option[_Value] =
    Ratio(Sum(p, q), _Rational.literalLike(2, Sum(p, q), env)).eval(env).toOption.flatMap { mid =>
      val replaced = parts.map { part =>
        Sum(Product(part.slope, mid), part.icpt).eval(env).toOption.flatMap(part.onPiece).map(r => (part.node, r))
      }
      sequence(replaced).flatMap { rs =>
        val flat = rs.foldLeft(e)((acc, nr) => replaceSubexpr(acc, nr._1, nr._2))
        _DefIntegral(flat, v, p, q).eval(env).toOption
      }
    }
