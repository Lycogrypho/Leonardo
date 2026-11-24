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
private[leonardo] object Staircase:

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
    val order  = compareBounds(lo, hi)
    if found.isEmpty || !a.isFinite || !b.isFinite || order == 0 then None
    else
      val (l, h) = if order < 0 then (lo, hi) else (hi, lo)
      // the tier of the whole computation: the integrand's, or the bounds' (a bare `x` has no exact
      // content, and `collect` builds its unit slope in its own tier, so it is read with an exact zero)
      val exact = _Rational.containsExactIn(e, env) || _Rational.containsExact(lo) || _Rational.containsExact(hi)
      for
        parts  <- sequence(found.map(part(_, v, exact, env)))
        jumps  <- jumpPoints(parts, l, h, env)
        sorted  = jumps.sortWith((p, q) => compareBounds(p, q) < 0)
        cuts    = l +: sorted.foldLeft(Vector.empty[_Value])((acc, p) =>
                    if acc.lastOption.exists(compareBounds(_, p) == 0) then acc else acc :+ p) :+ h
        values <- sequence(cuts.sliding(2).toList.map(pq => piece(e, v, parts, pq(0), pq(1), env)))
        total  <- values.reduceLeftOption((x, y) => Sum(x, y).eval(env).toOption.getOrElse(_Number(Double.NaN)))
        if num(total).isFinite
      yield if order < 0 then total
            else Product(_Rational.literalLike(-1, total), total).eval(env).toOption.getOrElse(total)

  /** The order of two bounds, `-1`, `0` or `1`: exactly when both are exact (F_0365), since `4` and
   *  `4 + 1/10^20` are one `Double`. */
  private def compareBounds(lo: _Value, hi: _Value): Int =
    val exact = if _Surd.isExact(lo) && _Surd.isExact(hi) then _Surd.compareExact(lo, hi) else None
    exact.getOrElse { val (a, b) = (num(lo), num(hi)); if a < b then -1 else if a > b then 1 else 0 }

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

  /** Why a staircase integral of `e` over `[lo, hi]` was not split, or `None` when it does not hold an
   *  integer part, or was (F_0348): an argument that is not linear, or more pieces than the budget.
   *  @param e   the integrand
   *  @param v   the integration variable
   *  @param lo  the lower limit, evaluated
   *  @param hi  the upper limit, evaluated
   *  @param env bindings and the budgets
   *  @return the reason, as text */
  def declineReason(e: _Expression, v: _Variable, lo: _Value, hi: _Value, env: Environment): Option[String] =
    val found = nodes(e, v)
    val (a, b) = (num(lo), num(hi))
    val order  = compareBounds(lo, hi)
    if found.isEmpty || !a.isFinite || !b.isFinite || order == 0 then None
    else
      val (l, h) = if order < 0 then (lo, hi) else (hi, lo)
      val exact  = _Rational.containsExactIn(e, env) || _Rational.containsExact(lo) || _Rational.containsExact(hi)
      found.find(part(_, v, exact, env).isEmpty) match
        case Some(node) =>
          Some(s"$node jumps where its argument crosses an integer, and those points are enumerated only " +
               s"for an argument linear in ${v.variable} with numeric coefficients")
        case None =>
          sequence(found.map(part(_, v, exact, env))).flatMap { parts =>
            val n = pieceCount(parts, l, h, env)
            Option.when(n.isFinite && n > env.limits(Limit.MaxStaircasePieces))(
              f"the integrand has ${n}%.0f pieces between its jumps, past the budget of " +
              s"${env.limits(Limit.MaxStaircasePieces)} (Limit.MaxStaircasePieces)")
          }

  /** For each part, the range of `k` whose jump `k` (`k + 1/2` for `round`) lies strictly inside the
   *  argument's range over `(l, h)`.  **Exact where the argument's values at the bounds are** (F_0365):
   *  read through `Double`, `4 + 1/10^20` is `4`, the jump at `4` was not seen as inside, and the piece
   *  beyond it was lost from an exact answer. */
  private def jumpRanges(parts: List[Part], l: _Value, h: _Value, env: Environment): List[(Part, Double, Double)] =
    def at(p: Part, x: _Value): Option[_Value] = Sum(Product(p.slope, x), p.icpt).eval(env).toOption
    def whole(e: _Expression): Option[Double] = e.eval(env).toOption.collect { case r: _Rational => num(r) }
    parts.map { p =>
      (at(p, l), at(p, h)) match
        case (Some(ga), Some(gb)) if _Surd.isExact(ga) && _Surd.isExact(gb) && _Surd.compareExact(ga, gb).isDefined =>
          val (lo, hi) = if _Surd.compareExact(ga, gb).exists(_ <= 0) then (ga, gb) else (gb, ga)
          val shift    = if p.halfIntegers then _Rational.of(BigInt(-1), BigInt(2)).getOrElse(_Rational(0)) else _Rational(0)
          (whole(Floor(Sum(lo, shift))), whole(Ceil(Sum(hi, shift)))) match
            case (Some(first), Some(last)) => (p, first + 1, last - 1)
            case _                         => (p, Double.NaN, Double.NaN)
        case (ga, gb) =>
          val (a, b) = (ga.fold(Double.NaN)(num), gb.fold(Double.NaN)(num))
          val shift  = if p.halfIntegers then 0.5 else 0.0
          (p, math.floor(math.min(a, b) - shift) + 1, math.ceil(math.max(a, b) - shift) - 1)
    }

  /** How many pieces the jumps cut `(l, h)` into. */
  private def pieceCount(parts: List[Part], l: _Value, h: _Value, env: Environment): Double =
    jumpRanges(parts, l, h, env).map((_, first, last) => math.max(0.0, last - first + 1)).sum + 1

  /** Every jump strictly inside `(l, h)`, in the tier of the bounds and coefficients, or `None` past the
   *  piece budget. */
  private def jumpPoints(parts: List[Part], l: _Value, h: _Value, env: Environment): Option[Vector[_Value]] =
    val budget = env.limits(Limit.MaxStaircasePieces)
    val ranges = jumpRanges(parts, l, h, env)
    val count  = pieceCount(parts, l, h, env) - 1
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
