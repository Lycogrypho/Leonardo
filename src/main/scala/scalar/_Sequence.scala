package it.grypho.scala.leonardo
package scalar

import core.*
import matrix._Matrix


/** Which linear recurrence a [[_Sequence]] denotes — issue 6.28.
 *
 *  Every case is `x(k) = p*x(k-1) + q*x(k-2)` with its own seeds and coefficients, so there
 *  is **one** definition of the recurrence and the named variants are spellings of it:
 *
 *  | kind | seeds `(a, b)` | `(p, q)` | first terms |
 *  |---|---|---|---|
 *  | `Fibonacci`  | `(0, 1)` | `(1, 1)` | 0, 1, 1, 2, 3, 5, ... |
 *  | `Lucas`      | `(2, 1)` | `(1, 1)` | 2, 1, 3, 4, 7, 11, ... |
 *  | `Pell`       | `(0, 1)` | `(2, 1)` | 0, 1, 2, 5, 12, 29, ... |
 *  | `Jacobsthal` | `(0, 1)` | `(1, 2)` | 0, 1, 1, 3, 5, 11, ... |
 *
 *  **An enum-tagged node rather than parse-time aliases**, which is the
 *  `_Statistic`/`StatKind` and `_Comparison`/`CompareOp` pattern.  Desugaring `lucas(n)` into
 *  `fib(n, 2, 1)` — the `dfact` -> `mfact` route — would have been cheaper, but the sugar is
 *  lost on the way back: the user types `lucas(10)` and the REPL, and `:save`, answer
 *  `fib(10, 2, 1)`.  Acceptable for `dfact`; self-defeating for a family whose point is that
 *  the names are related.  This way each name survives the round trip *and* the equivalence
 *  (`lucas(n) = fib(n, 2, 1)`) is a documented fact rather than a second implementation.
 */
enum SeqKind(val fnName: String, val a: Int, val b: Int, val p: Int, val q: Int):
  case Fibonacci  extends SeqKind("fib",        0, 1, 1, 1)
  case Lucas      extends SeqKind("lucas",      2, 1, 1, 1)
  case Pell       extends SeqKind("pell",       0, 1, 2, 1)
  case Jacobsthal extends SeqKind("jacobsthal", 0, 1, 1, 2)


/** A term of a linear recurrence: `fib(n)`, `lucas(n)`, `pell(n)`, `jacobsthal(n)`, and the
 *  seed-generalised `fib(n, a, b)` — issue 6.28.
 *
 *  **Indexing is the standard one**: `x(0) = 0`, `x(1) = 1`, so `fib(10) = 55`, matching OEIS
 *  A000045 and every published table.  The "classic rabbit" pair is `fib(n, 1, 1)`, which is
 *  this sequence shifted by one (`fib(n, 1, 1) = fib(n+1)`).  Stated here because it cannot
 *  be inferred and an off-by-one would be silent — the rule 6.26 follows for the polar angle.
 *
 *  **These are *numeric* sequences, not integer ones.**  The recurrence needs only addition
 *  and multiplication, so `fib(5, 1.5, -pi)` is a legitimate call and exact seeds stay exact.
 *  An exact integer index with exact seeds goes through [[linRecExact]], which is what lets
 *  `fib(79)` be right: `F(79)` exceeds `2^53`, so the `Double` path can only approximate it.
 *
 *  @param kind  which recurrence
 *  @param n     the index
 *  @param seeds empty for the kind's own seeds, or exactly two expressions overriding them
 */
case class _Sequence(kind: SeqKind, n: _Expression, seeds: List[_Expression] = Nil)
    extends _Expression:

  override def toString: String =
    if seeds.isEmpty then s"${kind.fnName}($n)"
    else s"${kind.fnName}($n, ${seeds.mkString(", ")})"

  override def children: List[_Expression] = n :: seeds
  override def rebuild(c: List[_Expression]): _Expression = _Sequence(kind, c.head, c.tail)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val seedValues = seeds.map(_.eval(env))
    if seedValues.exists(_.isLeft) then Left(this)
    else
      val vs = seedValues.collect { case Right(v) => v }
      n.eval(env) match
        // Exact index with exact seeds: the whole point of the exact path, since a Double
        // stops representing Fibonacci exactly at F(78).
        case Right(r: _Rational) =>
          val exact =
            for
              i  <- r.toBigIntExact
              ab <- exactSeeds(vs)
              x  <- linRecExact(i, ab._1, ab._2, _Rational(kind.p), _Rational(kind.q))
            yield Right(x)
          exact.getOrElse(inexact(r.toDouble, vs))
        case Right(_Number(x)) => inexact(x, vs)
        case _                 => Left(this)

  /** The seed pair as exact rationals: the node's own defaults, or two exact overrides. */
  private def exactSeeds(vs: List[_Value]): Option[(_Rational, _Rational)] = vs match
    case Nil                                     => Some((_Rational(kind.a), _Rational(kind.b)))
    case (x: _Rational) :: (y: _Rational) :: Nil => Some((x, y))
    case _                                       => None

  /** The `Double` path, used for an inexact index or inexact seeds. */
  private def inexact(x: Double, vs: List[_Value]): Either[_Expression, _Value] =
    val ab = vs match
      case Nil => Some((kind.a.toDouble, kind.b.toDouble))
      case _   => vs.map(v => _Complex.parts(v).collect { case (re, im) if im == 0.0 => re }) match
        case Some(p) :: Some(q) :: Nil => Some((p, q))
        case _                         => None
    ab.flatMap((a, b) => linRecOf(x, a, b, kind.p.toDouble, kind.q.toDouble))
      .map(d => Right(_Number(d)))
      .getOrElse(Left(this))


/** Upper bound on the number of terms [[_Tabulate]] will produce.
 *
 *  A row of a thousand values is already past what a REPL line can show; the cap exists so a
 *  mistyped bound declines instead of building an unbounded matrix, the same reasoning as
 *  `MaxTruthTableVars`.
 */
val MaxTabulateTerms: Int = 1000

/** `tabulate(e, k, lo, hi)` — evaluates `e` at each integer `k` in `[lo, hi]`, as a row
 *  matrix (issue 6.28).
 *
 *  **The generic answer to "how do I get a list of terms?"**  It is not specific to the
 *  sequences: `tabulate(binom(4, k), k, 0, 4)` is a Pascal row and `tabulate(k^2, k, 1, 5)`
 *  the squares, so every function in the library gains a tabulated form from one node.
 *
 *  The result is a `1xn` [[matrix._Matrix]], which is already this project's carrier for a
 *  sequence of values — `eigen(A)` returns one — so indexing with `at(r, 1, k)`, tuple
 *  assignment, the matrix operations, pretty-printing and `:save` all come for free.
 *
 *  `k` is a **binder**: excluded from `children` and carried through `rebuild`, so
 *  `substitute` cannot rewrite it, while `lo` and `hi` are ordinary children — the
 *  [[_DefIntegral]] convention.  The index is bound as a `_Number`, matching `_DefIntegral`
 *  and `sample`; a term that does not fold stays symbolic in its cell.
 *
 *  @param e  the expression to tabulate
 *  @param v  the index variable (a binder)
 *  @param lo the first index, inclusive
 *  @param hi the last index, inclusive
 */
case class _Tabulate(e: _Expression, v: _Variable, lo: _Expression, hi: _Expression)
    extends _Functional:

  override def toString: String = s"tabulate($e, $v, $lo, $hi)"
  override def children: List[_Expression] = List(e, lo, hi)
  override def rebuild(c: List[_Expression]): _Expression = _Tabulate(c.head, v, c(1), c(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val bounds =
      for
        l <- indexOf(lo, env)
        h <- indexOf(hi, env)
        if h >= l && (h - l + 1) <= MaxTabulateTerms
      yield (l, h)
    bounds match
      case None => Left(this)
      case Some((l, h)) =>
        val terms = (l to h).toVector.map { i =>
          e.eval(env.withBinding(v.variable, _Number(i.toDouble))).toExpression
        }
        Left(_Matrix(1, terms.size, terms))

  /** A bound as an integer, or `None` when it is not a concrete whole number. */
  private def indexOf(b: _Expression, env: Environment): Option[Int] =
    b.eval(env) match
      case Right(_Number(d)) if !d.isNaN && !d.isInfinite && d == Math.floor(d) &&
                                math.abs(d) <= Int.MaxValue => Some(d.toInt)
      case _                                                => None
