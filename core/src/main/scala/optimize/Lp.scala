package it.grypho.scala.leonardo
package optimize

import core.*
import spire.math.Rational as Q


/** Why [[lpSolve]] declined (F_0052 Decision A's mechanism: a reason at the Scala level). */
enum LpFailure:
  /** The problem as written is malformed; `detail` says how. */
  case InvalidInput(detail: String)
  /** No point satisfies the constraints — proved, since the arithmetic is exact. */
  case Infeasible
  /** The objective decreases without bound on the feasible set — proved likewise. */
  case Unbounded
  /** [[MaxLpPivots]] pivots were taken without reaching an answer. */
  case MaxIterations(limit: Int)
  /** An exact coefficient outgrew [[MaxLpBits]]: exact arithmetic does not round, it grows, and
   *  past this size each pivot costs seconds. */
  case TooLarge(bits: Int)

/** An optimal vertex of a linear program.
 *
 *  @param x          the minimiser: exact rationals when every coefficient was exact, `Double`s
 *                    otherwise (the exact optimum of the given `Double`s, rounded once at the end)
 *  @param objective  `fᵀx` there, in the same tier
 *  @param iterations simplex pivots, both phases
 *  @param active     the inequality rows and bounds holding with equality, named as in `qp`
 */
case class LpResult(x: Vector[_Value], objective: _Value, iterations: Int, active: Vector[QpConstraint])

/** The largest number of variables [[lpSolve]] accepts, checked before any work.
 *
 *  **Measured on Node, the slowest platform**, random dense integer problems over four seeds:
 *  at 40 variables and 80 constraints 0.05–0.3 s on the JVM and 0.1–1.7 s on Node; at 50 × 100
 *  one seed took 1.2 s on the JVM and 7.4 s on Node.  Exact arithmetic's cost is the size the
 *  rationals grow to rather than the dimension, which is why [[MaxLpBits]] was meant to bound
 *  the time and these two only the work per pivot.  **Neither does, as F_0105 measured**: dense
 *  integer problems at 40 × 80 with entries `0 … 9` and every cost negative take 25–217 pivots,
 *  3–139 s on Node and 0.5–26 s on the JVM, and never reach [[MaxLpBits]]; F_0115 holds the
 *  decision on a cap that bounds the time. */
val MaxLpVariables: Int = 40

/** The largest number of constraint rows (inequalities and equalities); see [[MaxLpVariables]]. */
val MaxLpConstraints: Int = 80

/** The pivot cap.  Bland's rule cannot cycle, but its worst case is exponential; at the sizes
 *  accepted the measured problems take tens of pivots. */
val MaxLpPivots: Int = 20000

/** The largest exact coefficient, in bits of numerator plus denominator, before [[lpSolve]]
 *  declines — meant as the cap that bounds the time, since the dimension does not; it does not
 *  bound it either (see [[MaxLpVariables]] and F_0115).
 *
 *  Measured on **every entry a pivot writes** (F_0105): the pivot row, each row it rewrites,
 *  the right-hand sides and the reduced costs.  The input is not measured on its own: a large
 *  coefficient costs nothing until a pivot combines it, and the first pivot that does is
 *  measured. */
val MaxLpBits: Int = 2048


/** Minimises `fᵀx` subject to `A x ≤ b`, `Aeq x = beq` and `lb ≤ x ≤ ub`, **exactly** (F_0011).
 *
 *  The two-phase simplex method over the rationals with **Bland's rule** — the lowest-index
 *  entering column and the lowest-index leaving variable on a tie — which cannot cycle, so a
 *  degenerate problem (Beale's example) terminates.  **Exact arithmetic is the point**, the
 *  entry's reason for building this rather than leaving LP to a numeric library: every pivot
 *  decision a floating-point simplex makes with a tolerance ("is this reduced cost negative?",
 *  "is this ratio a tie?") is decided exactly, so `Infeasible` and `Unbounded` are proofs, not
 *  guesses, and the optimum needs no certificate beyond the termination itself.  A `Double`
 *  coefficient is read as the dyadic rational it holds, and the answer is rounded to `Double`
 *  once, at the end.
 *
 *  When the optimum is not unique the vertex Bland's rule reaches is returned — deterministic,
 *  stated because another solver may return another optimal vertex.
 *
 *  @param f   the cost vector
 *  @param a   the inequality rows and their right-hand side, or `None`
 *  @param eq  the equality rows and their right-hand side, or `None`
 *  @param lb  lower bounds, `-inf` for none
 *  @param ub  upper bounds, `inf` for none
 *  @return the optimal vertex, or why there is none
 */
def lpSolve(f: Vector[_Value],
            a: Option[(Vector[Vector[_Value]], Vector[_Value])], eq: Option[(Vector[Vector[_Value]], Vector[_Value])],
            lb: Vector[_Value], ub: Vector[_Value]): Either[LpFailure, LpResult] =
  val n = f.size
  def bad(d: String): Either[LpFailure, LpResult] = Left(LpFailure.InvalidInput(d))
  val finite = (f ++ a.toList.flatMap((r, b) => r.flatten ++ b) ++ eq.toList.flatMap((r, b) => r.flatten ++ b))
  val bounds = lb ++ ub
  def rows(g: Option[(Vector[Vector[_Value]], Vector[_Value])]) = g.fold(0)(_._2.size)
  if n == 0 then bad("there must be at least one variable")
  else if n > MaxLpVariables then bad(s"at most $MaxLpVariables variables are accepted, not $n")
  else if lb.size != n || ub.size != n then bad(s"lb and ub must each have $n entries")
  else if List(a, eq).flatten.exists((r, b) => r.size != b.size || r.exists(_.size != n)) then
    bad(s"every constraint row must have $n entries, one right-hand side per row")
  else if rows(a) + rows(eq) > MaxLpConstraints then bad(s"at most $MaxLpConstraints constraints are accepted")
  else
    (sequence(finite.toList.map(rational(_, allowInfinite = false))), sequence(bounds.toList.map(rational(_, allowInfinite = true)))) match
      case (None, _) | (_, None) => bad("every coefficient must be a finite number, every bound a number or ±inf")
      case (Some(_), Some(_)) =>
        val exact = (finite ++ bounds.filterNot(isInfinite)).forall(_.isInstanceOf[_Rational])
        def q(v: _Value): Q = rational(v, allowInfinite = false).flatten.getOrElse(Q.zero)
        def bound(v: _Value): Option[Q] = rational(v, allowInfinite = true).flatten
        LpProblem(f.map(q), a.map((r, b) => (r.map(_.map(q)), b.map(q))), eq.map((r, b) => (r.map(_.map(q)), b.map(q))),
                  lb.map(bound), ub.map(bound)).solve.map(_.result(exact))

/** A value as a rational: `Some(Some(q))` for a finite number, `Some(None)` for an infinite
 *  bound when `allowInfinite`, `None` for anything else. */
private def rational(v: _Value, allowInfinite: Boolean): Option[Option[Q]] = v match
  case r: _Rational => Some(Some(Q(r.num, r.den)))
  case _Number(d) if d.isInfinite => Option.when(allowInfinite)(None)
  case _Number(d)   => _Rational.fromDouble(d).map(r => Some(Q(r.num, r.den)))
  case _            => None

private def isInfinite(v: _Value): Boolean = v match
  case _: _Rational => false
  case _Number(d)   => d.isInfinite
  case _            => false


/** The problem rewritten in standard form `min cᵀy, M y = r, y ≥ 0`, with the map back to `x`. */
private final case class LpProblem(f: Vector[Q], a: Option[(Vector[Vector[Q]], Vector[Q])],
                                   eq: Option[(Vector[Vector[Q]], Vector[Q])],
                                   lb: Vector[Option[Q]], ub: Vector[Option[Q]]):
  private val n = f.size

  /** `xᵢ = offsetᵢ + Σ coefficient·yⱼ`, one or two standard variables per original one. */
  private final case class Map1(offset: Q, terms: List[(Int, Q)])

  def solve: Either[LpFailure, Solved] =
    if (0 until n).exists(i => (lb(i), ub(i)) match { case (Some(l), Some(u)) => l > u; case _ => false }) then
      Left(LpFailure.Infeasible)
    else
      // The standard variables: a shifted variable per bounded-below or bounded-above x, two per free x.
      val (maps, ny) = (0 until n).foldLeft((Vector.empty[Map1], 0)) { case ((ms, k), i) =>
        (lb(i), ub(i)) match
          case (Some(l), _)    => (ms :+ Map1(l, List((k, Q.one))), k + 1)
          case (None, Some(u)) => (ms :+ Map1(u, List((k, -Q.one))), k + 1)
          case (None, None)    => (ms :+ Map1(Q.zero, List((k, Q.one), (k + 1, -Q.one))), k + 2)
      }
      def inY(row: Vector[Q]): (Vector[Q], Q) =
        val coeff = Array.fill(ny)(Q.zero)
        var shift = Q.zero
        for i <- 0 until n if !row(i).isZero do
          shift += row(i) * maps(i).offset
          for (j, c) <- maps(i).terms do coeff(j) += row(i) * c
        (coeff.toVector, shift)
      // Inequality rows: the user's, then a finite upper bound of a bounded-below variable.
      val userIneq  = a.toList.flatMap((rs, bs) => rs.indices.map(k => (rs(k), bs(k), Option(QpConstraint.Row(k)))))
      val upperRows = (0 until n).toList.flatMap(i => (lb(i), ub(i)) match
        case (Some(_), Some(u)) => List((Vector.tabulate(n)(j => if j == i then Q.one else Q.zero), u, Option.empty[QpConstraint]))
        case _                  => Nil)
      val ineq = (userIneq ++ upperRows).map { (row, b, id) =>
        val (c, shift) = inY(row)
        (c, b - shift, id)
      }
      val eqs = eq.toList.flatMap((rs, bs) => rs.indices.map { k =>
        val (c, shift) = inY(rs(k))
        (c, bs(k) - shift)
      })
      val (cy, cShift) = inY(f)
      Tableau.solve(cy, ineq.map((c, b, _) => (c, b)), eqs).map { (y, pivots) =>
        val x = maps.map(m => m.terms.foldLeft(m.offset)((acc, t) => acc + t._2 * y(t._1)))
        val slackZero = ineq.indices.map { k =>
          val (c, b, _) = ineq(k)
          c.indices.foldLeft(Q.zero)((acc, j) => acc + c(j) * y(j)) == b
        }
        val active =
          ineq.indices.toVector.collect { case k if slackZero(k) && ineq(k)._3.isDefined => ineq(k)._3 }.flatten ++
          (0 until n).toVector.flatMap { i =>
            lb(i).filter(_ == x(i)).map(_ => QpConstraint.Lower(i)).toVector ++
            ub(i).filter(_ == x(i)).map(_ => QpConstraint.Upper(i)).toVector
          }
        Solved(x, f.indices.foldLeft(Q.zero)((acc, i) => acc + f(i) * x(i)), pivots, active)
      }

/** A solved problem in exact rationals, before being written in the input's tier. */
private final case class Solved(x: Vector[Q], objective: Q, pivots: Int, active: Vector[QpConstraint]):
  def result(exact: Boolean): LpResult =
    def value(q: Q): _Value =
      if exact then _Rational.of(q.numerator.toBigInt, q.denominator.toBigInt).getOrElse(_Number(q.toDouble))
      else _Number(q.toDouble)
    LpResult(x.map(value), value(objective), pivots, active)


/** The two-phase dense simplex over the rationals, Bland's rule throughout. */
private object Tableau:

  /** Minimises `cᵀy` on `ineq` (`≤`) and `eqs` (`=`), `y ≥ 0`: the optimal `y` and the pivots. */
  def solve(c: Vector[Q], ineq: List[(Vector[Q], Q)], eqs: List[(Vector[Q], Q)]): Either[LpFailure, (Vector[Q], Int)] =
    val ny = c.size
    val ns = ineq.size
    // Rows `[coefficients | slacks | rhs]`, each negated if its right-hand side is negative so
    // that phase 1 starts from a feasible artificial basis.
    val raw: Vector[(Vector[Q], Q)] =
      ineq.zipWithIndex.map { case ((cs, b), k) => (cs ++ Vector.tabulate(ns)(j => if j == k then Q.one else Q.zero), b) }.toVector ++
      eqs.map((cs, b) => (cs ++ Vector.fill(ns)(Q.zero), b)).toVector
    val negated = raw.map(_._2.signum < 0)
    val rows    = raw.zip(negated).map { case ((cs, b), neg) => if neg then (cs.map(-_).toArray, -b) else (cs.toArray, b) }
    val m     = rows.size
    val width = ny + ns + m          // + one artificial per row
    val t     = Array.tabulate(m)(i => Array.tabulate(width)(j => if j < ny + ns then rows(i)._1(j) else if j - ny - ns == i then Q.one else Q.zero))
    val rhs   = rows.map(_._2).toArray
    // An inequality whose right-hand side was already non-negative starts with its slack basic,
    // the textbook start; only the others need an artificial, and an all-`≤` problem with
    // `b ≥ 0` needs no phase 1 at all.
    val basis = Array.tabulate(m)(i => if i < ns && !negated(i) then ny + i else ny + ns + i)
    var pivots = 0
    // The reduced-cost row of the phase being run, kept current by every pivot rather than
    // recomputed: with rationals the recomputation was most of the time.
    var red: Array[Q] = Array.empty

    // The widest coefficient written so far, in bits, against MaxLpBits.
    var widest = 0
    def bits(q: Q): Int = q.numerator.toBigInt.bitLength + q.denominator.toBigInt.bitLength

    /** Records `q` in `widest` and returns it.  Every entry a pivot writes goes through here —
     *  the pivot row, each row it rewrites, their right-hand sides and the reduced costs:
     *  measuring the pivot row alone let a rewritten row pass the cap unseen until it pivoted
     *  itself, if ever (F_0105). */
    def written(q: Q): Q =
      widest = math.max(widest, bits(q))
      q

    /** One pivot.  Only the pivot row's non-zero columns change anywhere, and the slack and
     *  artificial blocks keep most of a row zero, so only those are visited. */
    def pivot(r: Int, col: Int): Unit =
      val p  = t(r)(col)
      val nz = (0 until width).filter(j => !t(r)(j).isZero).toArray
      for j <- nz do t(r)(j) = written(t(r)(j) / p)
      rhs(r) = written(rhs(r) / p)
      for i <- 0 until m if i != r && !t(i)(col).isZero do
        val factor = t(i)(col)
        for j <- nz do t(i)(j) = written(t(i)(j) - factor * t(r)(j))
        rhs(i) = written(rhs(i) - factor * rhs(r))
      if red.nonEmpty && !red(col).isZero then
        val factor = red(col)
        for j <- nz do red(j) = written(red(j) - factor * t(r)(j))
      basis(r) = col
      pivots += 1

    /** Runs to optimality on `cost` over the columns `allowed`; `Left` when unbounded or capped. */
    def run(cost: Int => Q, allowed: Int => Boolean, live: Array[Boolean]): Either[LpFailure, Unit] =
      red = Array.tabulate(width)(j => (0 until m).filter(live).foldLeft(cost(j))((acc, i) => acc - cost(basis(i)) * t(i)(j)))
      var result: Option[Either[LpFailure, Unit]] = None
      while result.isEmpty do
        if pivots >= MaxLpPivots then result = Some(Left(LpFailure.MaxIterations(MaxLpPivots)))
        else if widest > MaxLpBits then result = Some(Left(LpFailure.TooLarge(widest)))
        else
          val inBasis = basis.toSet
          (0 until width).find(j => allowed(j) && !inBasis.contains(j) && red(j).signum < 0) match
            case None => result = Some(Right(()))
            case Some(col) =>
              val candidates = (0 until m).filter(i => live(i) && t(i)(col).signum > 0)
              if candidates.isEmpty then result = Some(Left(LpFailure.Unbounded))
              else
                val best = candidates.map(i => rhs(i) / t(i)(col)).min
                val r = candidates.filter(i => rhs(i) / t(i)(col) == best).minBy(basis(_))
                pivot(r, col)
      result.getOrElse(Right(()))

    val live = Array.fill(m)(true)
    // Phase 1: minimise the sum of the artificials.
    run(j => if j >= ny + ns then Q.one else Q.zero, _ => true, live).flatMap { _ =>
      val infeasibility = (0 until m).filter(i => basis(i) >= ny + ns).map(rhs(_)).foldLeft(Q.zero)(_ + _)
      if infeasibility.signum > 0 then Left(LpFailure.Infeasible)
      else
        // Drive every artificial still basic (at zero) out of the basis; a row with no other
        // non-zero entry is redundant and is retired.
        for i <- 0 until m if basis(i) >= ny + ns do
          (0 until ny + ns).find(j => !t(i)(j).isZero) match
            case Some(col) => pivot(i, col)
            case None      => live(i) = false
        // Phase 2: the real cost, artificials barred.
        run(j => if j < ny then c(j) else Q.zero, _ < ny + ns, live).map { _ =>
          val y = Array.fill(ny)(Q.zero)
          for i <- 0 until m if live(i) && basis(i) < ny do y(basis(i)) = rhs(i)
          (y.toVector, pivots)
        }
    }
