package it.grypho.scala.leonardo
package equation

import core.*
import scalar.*
import logic.{And, Or}


// The exact sign chart (F_0343, the owner's decision of 2026-11-12).
//
// In the exact tier a rational function with exact coefficients is charted with no floating point:
// the cut points are the real roots of `N·D`, found by `scalar.exactRealRoots` (rational, surd or
// isolated), the sign on each open interval is read at an exact rational point inside it, and at a cut
// point the function is undefined (a root of `D`), zero (a root of `N` only), or — for a requirement
// against a threshold — the sign of its neighbours.  The `Double` chart, which this replaces in the exact
// tier, merged roots up to `~10⁻⁴` apart: `(x - 1)·(x - 1 - 1/10^6) > 0` answered `x != 1`.  A root
// with no exact form keeps a decimal bound, read to `Double` precision from its exact isolation.

/** The line cut at `points` (ascending), as the elements `I₀, r₁, I₁, …, rₘ, Iₘ`, each in the set or
 *  not, rendered as a relation: each maximal run of elements as one comparison or a bounded pair, a run
 *  of one point as `v = r`.  With `notEqual`, a set that is every interval and no point is written as
 *  `v != r₁ and …`, the inequality solver's form (F_0188); the domain renderer writes intervals.
 *  @param points  the cut points, as expressions
 *  @param in      membership of the `2m + 1` elements
 *  @param v       the variable
 *  @param notEqual whether to use the `!=` form for the line less its points
 *  @return the relation */
private[leonardo] def renderRuns(points: Vector[_Expression], in: Vector[Boolean], v: _Variable,
                                 notEqual: Boolean): _Expression =
  val m = points.size
  if notEqual && m > 0 && in.indices.forall(k => in(k) == (k % 2 == 0)) then
    points.map(p => _Comparison(v, CompareOp.Ne, p): _Expression).reduce(And(_, _))
  else
    val runs = in.indices.foldLeft(List.empty[(Int, Int)]) { (acc, k) =>
      if !in(k) then acc
      else acc match
        case (s, e) :: rest if e == k - 1 => (s, k) :: rest
        case _                            => (k, k) :: acc
    }.reverse
    def lower(s: Int): Option[_Expression] =
      if s == 0 then None
      else if s % 2 == 1 then Some(_Comparison(v, CompareOp.Ge, points((s - 1) / 2)))
      else Some(_Comparison(v, CompareOp.Gt, points(s / 2 - 1)))
    def upper(e: Int): Option[_Expression] =
      if e == 2 * m then None
      else if e % 2 == 1 then Some(_Comparison(v, CompareOp.Le, points((e - 1) / 2)))
      else Some(_Comparison(v, CompareOp.Lt, points(e / 2)))
    val pieces = runs.map { (s, e) =>
      if s == e && s % 2 == 1 then _Equation(v, points((s - 1) / 2))
      else (lower(s), upper(e)) match
        case (Some(l), Some(u)) => And(l, u)
        case (Some(l), None)    => l
        case (None, Some(u))    => u
        case (None, None)       => _Bool(true)
    }
    pieces.reduceLeftOption[_Expression](Or(_, _)).getOrElse(_Bool(false))

/** A function read as `N/D` and charted on the cut points of an isolation: its sign on each interval and
 *  at each cut point, or `None` at a point where it is undefined.  `k` is its index among the charted
 *  functions: the isolation's factors are `N₀, D₀, N₁, D₁, …`. */
private final class Charted(iso: RootIsolation, k: Int):
  /** The sign on open interval `i` (never `0`: a sample is no root of `N·D`). */
  def onInterval(i: Int): Int = iso.intervalSigns(2 * k)(i) * iso.intervalSigns(2 * k + 1)(i)
  /** The sign at cut point `j` (0-based), or `None` where `D` vanishes. */
  def atPoint(j: Int): Option[Int] =
    if iso.vanishes(2 * k + 1)(j) then None
    else if iso.vanishes(2 * k)(j) then Some(0)
    else Some(onInterval(j))   // no root of N or D here: the sign of the interval on its left

/** The charts of several functions on one set of cut points, the roots of every `Nₖ·Dₖ`, or `None` when
 *  one is not an exact rational function or the isolation runs out of budget. */
private def chartAll(fs: Seq[_Expression], v: _Variable, env: Environment): Option[(Vector[ExactRoot], Vector[Charted])] =
  for
    fracs <- core.sequence(fs.toList.map(qFraction(_, v, env)))
    poly   = fracs.foldLeft(Vector(spire.math.Rational.one): QPoly)((p, nd) => qTimesPoly(qTimesPoly(p, nd._1), nd._2))
    iso   <- isolateRoots(poly, fracs.toVector.flatMap((n, d) => Vector(n, d)), env.limits(Limit.MaxExactRootWork))
  yield (iso.roots, fracs.indices.toVector.map(Charted(iso, _)))

/** `lhs <op> rhs` solved by the exact sign chart, or `None` when the difference is not a rational
 *  function with exact coefficients (F_0343).
 *  @param c   the comparison
 *  @param v   the variable
 *  @param env bindings and the budget
 *  @return the solution set */
private[equation] def exactChartTier(c: _Comparison, v: _Variable, env: Environment): Option[_Expression] =
  val difference = Sum(c.lhs, Product(_Rational.literalLike(-1, c, env), c.rhs))
  chartAll(Seq(difference), v, env).map { (roots, charts) =>
    val f = charts.head
    def holds(s: Int): Boolean = c.op match
      case CompareOp.Lt => s < 0
      case CompareOp.Le => s <= 0
      case CompareOp.Gt => s > 0
      case CompareOp.Ge => s >= 0
      case CompareOp.Ne => s != 0
    val in = Vector.tabulate(2 * roots.size + 1) { k =>
      if k % 2 == 0 then holds(f.onInterval(k / 2)) else f.atPoint((k - 1) / 2).exists(holds)
    }
    renderRuns(roots.map(_.expression), in, v, notEqual = true)
  }

/** The set where every constraint holds, by the exact sign chart, or `None` when a constraint is not a
 *  rational function with exact coefficients or has a requirement this chart does not read (F_0343).
 *  @param cs      the constraints
 *  @param v       the variable
 *  @param env     bindings and the budget
 *  @param putBack whether an isolated excluded point belongs to the set after all: the differentiable
 *                 domain's smooth points (F_0278), which no constraint describes
 *  @return the set, written as intervals */
private[leonardo] def exactDomainChart(cs: Vector[scalar.Constraint], v: _Variable, env: Environment,
                                       putBack: Double => Boolean): Option[_Expression] =
  import scalar.Requirement.*
  val thresholds: Option[Vector[(scalar.Constraint, Int)]] = core.sequence(cs.toList.map(c => c.req match
    case Positive | NonNegative | NonZero => Some(Vector((c, 0)))
    case InClosedUnit                    => Some(Vector((c, -1), (c, 1)))
    case _                               => None)).map(_.toVector.flatten)
  for
    ts <- thresholds
    if ts.nonEmpty
    fs  = ts.map((c, t) => Sum(c.arg, _Rational(-t)))
    (roots, charts) <- chartAll(fs, v, env)
  yield
    /** Whether every requirement holds, given each function's sign (`None`: undefined). */
    def holds(sign: Int => Option[Int]): Boolean =
      ts.indices.forall { i =>
        val (c, t) = ts(i)
        sign(i).exists(s => c.req match
          case Positive     => s > 0
          case NonNegative  => s >= 0
          case NonZero      => s != 0
          case InClosedUnit => if t < 0 then s >= 0 else s <= 0
          case _            => false)
      }
    val byConstraints = Vector.tabulate(2 * roots.size + 1) { k =>
      if k % 2 == 0 then holds(i => Some(charts(i).onInterval(k / 2)))
      else holds(i => charts(i).atPoint((k - 1) / 2))
    }
    val in = byConstraints.indices.toVector.map { k =>
      byConstraints(k) || (k % 2 == 1 && byConstraints(k - 1) && byConstraints(k + 1) && putBack(roots((k - 1) / 2).toDouble))
    }
    renderRuns(roots.map(_.expression), in, v, notEqual = false)
