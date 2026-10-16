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
  /** The arithmetic reached [[MaxLpWork]] units before an answer (F_0115): the budget that
   *  bounds the time, where the dimension and bit caps do not. */
  case MaxWork(budget: Long)

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
 *  one seed took 1.2 s on the JVM and 7.4 s on Node.  **The dimension does not bound the time**,
 *  and neither does [[MaxLpBits]] (F_0105): dense integer problems at 40 × 80 took 3–139 s on
 *  Node in spire rationals, and never reached it. [[MaxLpWork]] is the cap that does (F_0115);
 *  these are input validation.
 *
 *  This and the caps below are the **defaults** of their [[core.Limit]]s (F_0124): a
 *  library caller raises one through [[core.Environment.withLimit]] or the [[lpSolve]] overload
 *  taking [[core.Limits]]. */
val MaxLpVariables: Int = Limit.MaxLpVariables.default.toInt

/** The largest number of constraint rows (inequalities and equalities); see [[MaxLpVariables]]. */
val MaxLpConstraints: Int = Limit.MaxLpConstraints.default.toInt

/** The pivot cap.  Bland's rule cannot cycle, but its worst case is exponential; at the sizes
 *  accepted the measured problems take tens of pivots. */
val MaxLpPivots: Int = Limit.MaxLpPivots.default.toInt

/** The widest integer, in bits, the fraction-free tableau may hold before [[lpSolve]] declines:
 *  an entry or the common denominator, the actual entries being their quotients (F_0115). A
 *  bound on memory and on one pivot's cost; the time is bounded by [[MaxLpWork]].
 *
 *  Measured on **every entry a pivot writes** (F_0105): every rewritten row, the right-hand
 *  sides and the reduced costs.  The input is not measured on its own: a large coefficient costs
 *  nothing until a pivot combines it, and the first pivot that does is measured. */
val MaxLpBits: Int = Limit.MaxLpBits.default.toInt

/** The arithmetic [[lpSolve]] may do, in work units, before it declines `MaxWork` (F_0115):
 *  **the cap that bounds the time**.
 *
 *  Each tableau entry a pivot rewrites, `(p·a − f·b)/d`, costs `80 + w(p)·w(a) + w(f)·w(b) +
 *  w(q)·w(d)` units, `w` counting 32-bit words and `q` the quotient: schoolbook multiplication
 *  and division, plus a fixed cost per entry. **Measured on Node**, over F_0105's dense problems
 *  (sixteen at 30 × 60 and 40 × 80): a unit is ~16–20 ns there and the fixed cost ~1.5 µs, which
 *  a micro-benchmark of `BigInteger` arithmetic confirms. At this default a decline comes after
 *  **2.1 s on Node** and 0.55 s on the JVM (the `MaxLsqWork` figure); fifteen of the sixteen
 *  answer, the slowest in 1.85 s on Node, and the sixteenth — 352 pivots, 7.7 s to answer — is
 *  declined. Checked after each rewritten row, so a decline overshoots by at most one row. */
val MaxLpWork: Long = Limit.MaxLpWork.default


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
  lpSolve(f, a, eq, lb, ub, Limits.Default)

/** [[lpSolve]] under `limits` (F_0124): the dimension, pivot, coefficient and work caps are
 *  [[core.Limit.MaxLpVariables]], `MaxLpConstraints`, `MaxLpPivots`, `MaxLpBits` and
 *  `MaxLpWork`. An overload
 *  rather than a defaulted parameter, which would be a binary break.
 *
 *  @param f      the cost vector
 *  @param a      the inequality rows and their right-hand side, or `None`
 *  @param eq     the equality rows and their right-hand side, or `None`
 *  @param lb     lower bounds, `-inf` for none
 *  @param ub     upper bounds, `inf` for none
 *  @param limits the budgets the solve runs under
 *  @return the optimal vertex, or why there is none
 */
def lpSolve(f: Vector[_Value],
            a: Option[(Vector[Vector[_Value]], Vector[_Value])], eq: Option[(Vector[Vector[_Value]], Vector[_Value])],
            lb: Vector[_Value], ub: Vector[_Value], limits: Limits): Either[LpFailure, LpResult] =
  val n = f.size
  val (maxVars, maxRows) = (limits.count(Limit.MaxLpVariables), limits.count(Limit.MaxLpConstraints))
  def bad(d: String): Either[LpFailure, LpResult] = Left(LpFailure.InvalidInput(d))
  val finite = (f ++ a.toList.flatMap((r, b) => r.flatten ++ b) ++ eq.toList.flatMap((r, b) => r.flatten ++ b))
  val bounds = lb ++ ub
  def rows(g: Option[(Vector[Vector[_Value]], Vector[_Value])]) = g.fold(0)(_._2.size)
  if n == 0 then bad("there must be at least one variable")
  else if n > maxVars then bad(s"at most $maxVars variables are accepted, not $n")
  else if lb.size != n || ub.size != n then bad(s"lb and ub must each have $n entries")
  else if List(a, eq).flatten.exists((r, b) => r.size != b.size || r.exists(_.size != n)) then
    bad(s"every constraint row must have $n entries, one right-hand side per row")
  else if rows(a) + rows(eq) > maxRows then bad(s"at most $maxRows constraints are accepted")
  else
    (sequence(finite.toList.map(rational(_, allowInfinite = false))), sequence(bounds.toList.map(rational(_, allowInfinite = true)))) match
      case (None, _) | (_, None) => bad("every coefficient must be a finite number, every bound a number or ±inf")
      case (Some(_), Some(_)) =>
        val exact = (finite ++ bounds.filterNot(isInfinite)).forall(_.isInstanceOf[_Rational])
        def q(v: _Value): Q = rational(v, allowInfinite = false).flatten.getOrElse(Q.zero)
        def bound(v: _Value): Option[Q] = rational(v, allowInfinite = true).flatten
        LpProblem(f.map(q), a.map((r, b) => (r.map(_.map(q)), b.map(q))), eq.map((r, b) => (r.map(_.map(q)), b.map(q))),
                  lb.map(bound), ub.map(bound)).solve(limits).map(_.result(exact))

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

  def solve(limits: Limits): Either[LpFailure, Solved] =
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
      Tableau.solve(cy, ineq.map((c, b, _) => (c, b)), eqs, limits).map { (y, pivots) =>
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


/** The two-phase dense simplex, **fraction-free over the integers** (F_0115), Bland's rule
 *  throughout.
 *
 *  The tableau is kept as integers `T` over one common denominator `d > 0`, the actual entries
 *  being `T/d`: Edmonds' integer-preserving form of Gauss–Jordan, Bareiss's for elimination. A
 *  pivot on `p = T(r)(col)` rewrites every other row, the reduced costs included, as
 *  `(p·a − f·b)/d` with `f` that row's entry in `col`, and the division is **exact**: each
 *  entry is a minor of the original integer matrix and `d` the basis's determinant. Then `d = p`.
 *  It replaced arithmetic in spire's `Rational`, whose cost the survey found was set not by the
 *  size of the coefficients but by its representation: every operation on a fraction pays a gcd,
 *  15–100× a whole number of the same size, and a value that fits a `Long` while its product
 *  does not runs 20–300× slower than a `BigInteger`. On `BigInteger` with no gcd the cost is
 *  that of the multiplications and the division, which [[MaxLpWork]] counts: the dense 40 × 80
 *  problems that took 3–139 s on Node take 1.0–7.7 s, the same pivots in the same order. */
private object Tableau:
  import java.math.BigInteger as Z

  /** The fixed cost of rewriting one entry, in work units: measured on Node, where allocating
   *  and dividing a `BigInteger` costs ~1.5 µs whatever its size and a unit ~16–20 ns. */
  private val EntryWork = 80L

  /** `cs` scaled by the least common multiple of its denominators: the same constraint, or a
   *  positive multiple of the same cost, in integers. */
  private def integral(cs: Vector[Q]): Vector[Z] =
    def big(s: spire.math.SafeLong): Z = s.toBigInt.bigInteger
    val l = cs.foldLeft(Z.ONE)((acc, q) => { val d = big(q.denominator); acc.divide(acc.gcd(d)).multiply(d) })
    cs.map(q => big(q.numerator).multiply(l.divide(big(q.denominator))))

  /** Minimises `cᵀy` on `ineq` (`≤`) and `eqs` (`=`), `y ≥ 0`: the optimal `y` and the pivots. */
  def solve(c: Vector[Q], ineq: List[(Vector[Q], Q)], eqs: List[(Vector[Q], Q)],
            limits: Limits): Either[LpFailure, (Vector[Q], Int)] =
    val (maxPivots, maxBits) = (limits.count(Limit.MaxLpPivots), limits.count(Limit.MaxLpBits))
    val maxWork = limits(Limit.MaxLpWork)
    val ny = c.size
    val ns = ineq.size
    // Each row `[coefficients | rhs]` in integers, negated if its right-hand side is negative so
    // that phase 1 starts from a feasible artificial basis.
    val raw     = (ineq ++ eqs).map((cs, b) => integral(cs :+ b)).toVector
    val negated = raw.map(_.last.signum < 0)
    val rows    = raw.zip(negated).map((row, neg) => if neg then row.map(_.negate) else row)
    val m     = rows.size
    val width = ny + ns + m          // + one artificial per row
    val t = Array.tabulate(m) { i =>
      Array.tabulate(width) { j =>
        if j < ny then rows(i)(j)
        else if j < ny + ns then
          if j - ny != i then Z.ZERO else if negated(i) then Z.ONE.negate else Z.ONE
        else if j - ny - ns == i then Z.ONE
        else Z.ZERO
      }
    }
    val rhs = rows.map(_.last).toArray
    // An inequality whose right-hand side was already non-negative starts with its slack basic,
    // the textbook start; only the others need an artificial, and an all-`≤` problem with
    // `b ≥ 0` needs no phase 1 at all.
    val basis = Array.tabulate(m)(i => if i < ns && !negated(i) then ny + i else ny + ns + i)
    var d      = Z.ONE
    var pivots = 0
    // The reduced-cost row of the phase being run, times `d`, kept current by every pivot.
    var red: Array[Z] = Array.empty

    // The widest integer written so far, in bits, against MaxLpBits; the work done, against MaxLpWork.
    var widest = 0
    var work   = 0L
    def words(z: Z): Long = (z.bitLength >>> 5) + 1L

    /** One pivot: every other row, and the reduced costs, become `(p·a − f·b)/d`. The pivot row
     *  itself is unchanged, since its actual entries `T(r)/d` become `T(r)/p`. Stops part-way
     *  once the work passes the budget, leaving a tableau `run` then declines on. */
    def pivot(r: Int, col: Int): Unit =
      val p  = t(r)(col)
      val pr = t(r)
      /** `(p·a − f·b)/d`, exact, recorded in `widest` and `work`. A zero that stays zero is not
       *  rewritten. */
      def combine(a: Z, f: Z, b: Z): Z =
        if a.signum == 0 && (f.signum == 0 || b.signum == 0) then a
        else
          val num = p.multiply(a).subtract(f.multiply(b))
          val q   = num.divide(d)
          work += EntryWork + words(p) * words(a) + words(f) * words(b) + words(q) * words(d)
          widest = math.max(widest, q.bitLength)
          q
      var i = 0
      while i < m && work <= maxWork do
        val row = t(i)
        val f   = row(col)
        if i != r && !(f.signum == 0 && p == d) then
          for j <- 0 until width do row(j) = combine(row(j), f, pr(j))
          rhs(i) = combine(rhs(i), f, rhs(r))
        i += 1
      if red.nonEmpty then
        val f = red(col)
        if !(f.signum == 0 && p == d) then
          for j <- 0 until width do red(j) = combine(red(j), f, pr(j))
      d = p
      if d.signum < 0 then
        // A negative pivot (only when driving an artificial out): negate everything, so `d > 0`
        // and an entry's sign is its actual sign.
        for row <- t; j <- 0 until width do row(j) = row(j).negate
        for i <- 0 until m do rhs(i) = rhs(i).negate
        for j <- red.indices do red(j) = red(j).negate
        d = d.negate
      widest = math.max(widest, d.bitLength)
      basis(r) = col
      pivots += 1

    /** Runs to optimality on `cost` over the columns `allowed`; `Left` when unbounded or capped. */
    def run(cost: Int => Z, allowed: Int => Boolean, live: Array[Boolean]): Either[LpFailure, Unit] =
      red = Array.tabulate(width)(j =>
        (0 until m).filter(live).foldLeft(d.multiply(cost(j))) { (acc, i) =>
          work += EntryWork + words(cost(basis(i))) * words(t(i)(j))
          acc.subtract(cost(basis(i)).multiply(t(i)(j)))
        })
      var result: Option[Either[LpFailure, Unit]] = None
      while result.isEmpty do
        if pivots >= maxPivots then result = Some(Left(LpFailure.MaxIterations(maxPivots)))
        else if widest > maxBits then result = Some(Left(LpFailure.TooLarge(widest)))
        // Load-bearing beyond the budget: a pivot the budget stopped part-way leaves the tableau
        // inconsistent, and this check is what keeps anything from reading it (with it removed,
        // a stopped pivot led on to a wrong optimum — the mutation test).
        else if work > maxWork then result = Some(Left(LpFailure.MaxWork(maxWork)))
        else
          val inBasis = basis.toSet
          (0 until width).find(j => allowed(j) && !inBasis.contains(j) && red(j).signum < 0) match
            case None => result = Some(Right(()))
            case Some(col) =>
              val candidates = (0 until m).filter(i => live(i) && t(i)(col).signum > 0)
              if candidates.isEmpty then result = Some(Left(LpFailure.Unbounded))
              else
                // The least ratio rhs(i)/T(i)(col), compared by cross-multiplication (both
                // denominators positive); a tie goes to the lowest basic variable.
                def ratio(i: Int, k: Int): Int = rhs(i).multiply(t(k)(col)).compareTo(rhs(k).multiply(t(i)(col)))
                val r = candidates.reduce { (i, k) =>
                  val cmp = ratio(i, k)
                  if cmp < 0 || (cmp == 0 && basis(i) < basis(k)) then i else k
                }
                pivot(r, col)
      result.getOrElse(Right(()))

    val live = Array.fill(m)(true)
    // Phase 1: minimise the sum of the artificials.
    run(j => if j >= ny + ns then Z.ONE else Z.ZERO, _ => true, live).flatMap { _ =>
      val infeasibility = (0 until m).filter(i => basis(i) >= ny + ns).map(rhs(_)).foldLeft(Z.ZERO)(_.add(_))
      if infeasibility.signum > 0 then Left(LpFailure.Infeasible)
      else
        // Drive every artificial still basic (at zero) out of the basis; a row with no other
        // non-zero entry is redundant and is retired.
        for i <- 0 until m if basis(i) >= ny + ns do
          (0 until ny + ns).find(j => t(i)(j).signum != 0) match
            case Some(col) => pivot(i, col)
            case None      => live(i) = false
        // Phase 2: the real cost, scaled to integers, artificials barred.
        val cz = integral(c)
        run(j => if j < ny then cz(j) else Z.ZERO, _ < ny + ns, live).map { _ =>
          val y = Array.fill(ny)(Q.zero)
          for i <- 0 until m if live(i) && basis(i) < ny do y(basis(i)) = Q(BigInt(rhs(i)), BigInt(d))
          (y.toVector, pivots)
        }
    }
