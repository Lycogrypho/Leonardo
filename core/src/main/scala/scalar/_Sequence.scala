package it.grypho.scala.leonardo
package scalar

import core.*
import matrix._Matrix


/** Which linear recurrence a [[_Sequence]] denotes.
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
 *  seed-generalised `fib(n, a, b)`.
 *
 *  **Indexing is the standard one**: `x(0) = 0`, `x(1) = 1`, so `fib(10) = 55`, matching OEIS
 *  [[https://oeis.org/A000045 A000045]] and every published table.  The "classic rabbit" pair is `fib(n, 1, 1)`, which is
 *  this sequence shifted by one (`fib(n, 1, 1) = fib(n+1)`).  Stated here because it cannot
 *  be inferred and an off-by-one would be silent — the rule also applied to the spherical polar angle.
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
 *  matrix.
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
    // A row is a GRID, so an empty one is not a meaningful answer -- unlike a reduction,
    // whose empty range has the identity.  Hence the `h >= l` filter here and not there.
    indexRange(lo, hi, env).filter((l, h) => h >= l) match
      case None => Left(this)
      case Some((l, h)) =>
        val terms = termsOf(e, v, l, h, env)
        Left(_Matrix(1, terms.size, terms))


/** A bound as an integer, or `None` when it is not a concrete whole number. */
private def indexOf(b: _Expression, env: Environment): Option[Int] =
  b.eval(env) match
    case Right(_Number(d)) if !d.isNaN && !d.isInfinite && d == Math.floor(d) &&
                              math.abs(d) <= Int.MaxValue => Some(d.toInt)
    case _                                                => None

/** Both bounds as integers, refusing a range longer than [[MaxTabulateTerms]].
 *
 *  Shared by [[_Tabulate]] and [[_Reduction]] rather than written twice: the cap is one
 *  decision about how many terms a mistyped bound may build, and two copies of it would only
 *  be a way for the two nodes to disagree about what is too many.
 */
private def indexRange(lo: _Expression, hi: _Expression, env: Environment): Option[(Int, Int)] =
  for
    l <- indexOf(lo, env)
    h <- indexOf(hi, env)
    if (h - l + 1) <= MaxTabulateTerms
  yield (l, h)

/** Evaluates `e` at each index in `[l, h]`, with `v` bound to that index.
 *
 *  **The index is built in the TERM's tier, not as a bare `_Number`** — the numeric-tier rule.
 *  An index is a constant this traversal invents, so binding `1.0` into `sum(1/k, k, 1, 3)`
 *  would demote every term through float contagion and the exact answer `11/6` would come
 *  back as a `Double`, silently.  `literalLike` is what keeps the index in step with the term
 *  it is about to be substituted into; outside exact mode it yields the same `_Number` as
 *  before, so the inexact path is unchanged.
 *
 *  A term that does not fold stays symbolic, which is what lets a reduction expand rather than
 *  refuse.
 */
private def termsOf(e: _Expression, v: _Variable, l: Int, h: Int,
                    env: Environment): Vector[_Expression] =
  (l to h).toVector.map(i =>
    e.eval(env.withBinding(v.variable, _Rational.literalLike(i, e))).toExpression)


/** Which reduction a [[_Reduction]] denotes (issue F_0037).
 *
 *  Two kinds, one definition: they differ only in the binary node that combines two terms and
 *  in what an empty range is worth, so [[_Reduction]] holds both facts once and the named
 *  spellings round-trip -- the `_Sequence`/`SeqKind` pattern.
 */
enum ReduceKind(val word: String):
  /** `sum(e, k, lo, hi)` -- the terms added; the empty sum is `0`. */
  case Sum extends ReduceKind("sum")
  /** `product(e, k, lo, hi)` -- the terms multiplied; the empty product is `1`. */
  case Product extends ReduceKind("product")


/** `sum(e, k, lo, hi)` / `product(e, k, lo, hi)` -- a finite reduction over an integer range.
 *
 *  **The counterpart of [[_Tabulate]], which yields the terms rather than their total**, and
 *  the reason a summation could not be converted from the editor before: `tabulate` is not an
 *  answer to a `Σ`, so the AsciiMath reader had nothing to translate one into and refused it
 *  by name.  Same shape as `tabulate` -- `k` is a **binder** (excluded from `children`, carried
 *  through `rebuild`, so `substitute` cannot rewrite it) while `lo`/`hi` are ordinary children,
 *  the [[_DefIntegral]] convention -- and the same `MaxTabulateTerms` cap, shared through
 *  [[indexRange]].
 *
 *  **The terms are folded with `reduce`, never a seeded `fold`.**  Seeding with a literal
 *  `_Number(0)` would be an *inexact-tier* constant, so an exact sum would be demoted through
 *  float contagion before anything could use it -- the numeric-tier rule, and the single most
 *  repeated bug of issues 4.L and 4.N.  The identity is therefore used for the **empty range
 *  only**, where there is nothing to combine with and nothing to demote.
 *
 *  **An empty range is the identity rather than a refusal**, unlike `tabulate`'s empty grid:
 *  the empty sum is 0 and the empty product 1 by universal convention, which is exactly what
 *  makes `sum(f, k, 1, n)` read correctly at `n = 0` instead of declining a base case.
 *
 *  **Symbolic terms EXPAND.**  `sum(k*x, k, 1, 3)` becomes `x + 2x + 3x` rather than staying
 *  folded, so `normalize`/`simplify` can collect it -- and since this is a `_Functional`,
 *  F_0034's pass reduces it under the `simplify` command with no further wiring.
 *
 *  @param kind which reduction this is
 *  @param e    the term, in which `v` is bound
 *  @param v    the index variable (a binder)
 *  @param lo   the first index, inclusive
 *  @param hi   the last index, inclusive
 */
case class _Reduction(kind: ReduceKind, e: _Expression, v: _Variable,
                      lo: _Expression, hi: _Expression) extends _Functional:

  override def toString: String = s"${kind.word}($e, $v, $lo, $hi)"
  override def children: List[_Expression] = List(e, lo, hi)
  override def rebuild(c: List[_Expression]): _Expression = _Reduction(kind, c.head, v, c(1), c(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    indexRange(lo, hi, env) match
      case None                  => Left(this)
      case Some((l, h)) if l > h => Right(identity)
      case Some((l, h))          =>
        val terms = termsOf(e, v, l, h, env)
        // Accumulated FLAT: each step combines the running result with one term and evaluates
        // immediately, so a numeric fold never builds the n-deep left spine a single
        // `reduce`-then-`eval` would -- and a depth bound has to hold on the shallowest
        // platform's stack, not the JVM's (the `Parser.MaxDepth` rule).
        terms.reduce((acc, t) => combine(acc, t).eval(env).toExpression).eval(env)

  /** The two terms combined -- the one place this kind's operator is named. */
  private def combine(a: _Expression, b: _Expression): _Expression = kind match
    case ReduceKind.Sum     => Sum(a, b)
    case ReduceKind.Product => Product(a, b)

  /** What an empty range is worth: the operator's identity. */
  private def identity: _Value = kind match
    case ReduceKind.Sum     => _Number(0)
    case ReduceKind.Product => _Number(1)
