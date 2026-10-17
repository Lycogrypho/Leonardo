package it.grypho.scala.leonardo
package probability

import core.*


/** Why [[drawSamples]] or [[prbsSequence]] declined (F_0052 Decision A: the reason travels as data,
 *  the REPL prints it).  The node itself stays as written. */
enum SamplingFailure:
  /** The first argument of `draw` did not evaluate to a distribution. */
  case NotADistribution
  /** A seed or an offset is not a whole number a `Double` represents exactly (`|s| ≤ 2⁵³`), or an
   *  offset is negative; `what` names which. */
  case InvalidInteger(what: String)
  /** The count is below 1 or past the cap for this call. */
  case InvalidCount(n: Double, cap: Int)
  /** The PRBS order is outside the verified table, [[MinPrbsOrder]] … [[MaxPrbsOrder]]. */
  case InvalidOrder(order: Double)
  /** The inverse cdf had no value at the uniform drawn for sample `index`. */
  case Undefined(index: Int)


/** The most samples one `draw` produces (F_0067).
 *
 *  **Measured** (2026-10-10) at the cap, JVM / Node: `normal(0, 1)` 1.8 / 3.3 s, `poisson(4)`
 *  0.8 / 2.2 s, `binomial(20, 0.3)` 3.0 / 6.9 s — the slowest family on the slowest platform.  An
 *  earlier probe on evenly spaced probabilities predicted 4.4 s for that last figure: random
 *  uniforms reach the tails, where the bisection's bracket takes longer to set.  T3-03's own
 *  acceptance needs `10⁵` normal draws, which is why the cap is not lower; the figure is also
 *  `Session.MaxSampleCount`'s, the other materialised sequence a user asks for by count.
 *
 *  The **default** of [[core.Limit.MaxDrawCount]] (F_0124): the `Limits` overload of
 *  [[drawSamples]] and the `draw` node read the caller's value.
 */
val MaxDrawCount: Int = Limit.MaxDrawCount.default.toInt

/** The most terms one `prbs` produces (F_0067): a step is a few integer operations, so the
 *  cost is the result itself — a million `Double`s, eight megabytes.  **Measured** at the cap:
 *  under 0.35 s on the JVM and on Node, through the library function and through the node.
 *  The **default** of [[core.Limit.MaxPrbsLength]] (F_0124). */
val MaxPrbsLength: Int = Limit.MaxPrbsLength.default.toInt

/** The lowest PRBS order (F_0067): order 1 has no maximal sequence worth the name. */
val MinPrbsOrder: Int = 2

/** The highest PRBS order (F_0067).  The table stops here because every entry is **verified by
 *  running its full period** in the suite — `2²⁰ − 1` steps is cheap on Node, `2³¹` is not, and
 *  an unverified primitive polynomial is a transcription that only looks right. */
val MaxPrbsOrder: Int = 20


/** The library's own generator: SplitMix64 (F_0067).
 *
 *  A 64-bit counter advanced by a fixed odd constant and finished by a mixing function — passes
 *  BigCrush, needs no per-platform code, and is exact on Scala.js, which emulates `Long` exactly.
 *  **Never `scala.util.Random`**: a platform generator compiles on both platforms and diverges at
 *  run time, the `DoubleRender` rule.  The output for `(seed, i)` is a pure function of both, so
 *  a stream needs no state to continue: the `i`-th value is computed directly.
 */
private[probability] object SplitMix64:
  /** The counter's increment, `2⁶⁴/φ` rounded to odd. */
  val Golden: Long = 0x9E3779B97F4A7C15L

  /** The finalising bijection (Stafford's variant 13). */
  def mix(z0: Long): Long =
    var z = z0
    z = (z ^ (z >>> 30)) * 0xBF58476D1CE4E5B9L
    z = (z ^ (z >>> 27)) * 0x94D049BB133111EBL
    z ^ (z >>> 31)

  /** The `i`-th 64-bit output of the stream for `seed`.  The seed is **hashed** into the start of
   *  the counter rather than used as a position on it: a seed taken as a position would make the
   *  stream of `seed + 1` the stream of `seed` shifted by one, so two "independent" runs would
   *  share all but one value. */
  def at(seed: Long, i: Long): Long = mix(mix(seed) + (i + 1) * Golden)

  /** The `i`-th uniform in the open interval `(0, 1)`: the top 53 bits, centred in their cell, so
   *  neither `0` nor `1` can occur — the inverse cdf is undefined at both.  Exact in a `Double` on
   *  either platform. */
  def uniform(seed: Long, i: Long): Double =
    ((at(seed, i) >>> 11).toDouble + 0.5) * 1.1102230246251565e-16   // 2^-53


/** `n` draws from `d`, the `offset`-th of the stream for `seed` onwards (F_0067, MPC T3-03).
 *
 *  **By inversion**: each draw is the quantile of one uniform, through the same `quantileOf` the
 *  `quantile` query uses — so no family needs a sampler of its own, and a family added later can be
 *  drawn from as soon as it has a cdf.  A discrete family yields integers.
 *
 *  **The seed is the state** (Decision B): nothing is remembered between calls, and
 *  `drawSamples(d, n, s, o) ++ drawSamples(d, m, s, o + n)` is `drawSamples(d, n + m, s, o)`
 *  exactly — a long stream can be taken in pieces.  A control loop that wants fresh noise each
 *  cycle passes the cycle number as the seed, or the running count as the offset.
 *
 *  **Reproducibility** (Decision C): the uniforms are bit-identical on the JVM and Scala.js.  A
 *  draw goes through the family's cdf, whose `exp` and `log` each platform computes itself, so a
 *  continuous draw agrees across platforms to rounding (well within `1e-12` relative) rather than
 *  to the bit.
 *
 *  @param d      the distribution
 *  @param n      how many draws, `1 … MaxDrawCount`
 *  @param seed   the seed, any integer
 *  @param offset the position in the stream to start from, `≥ 0`
 *  @return the draws, or why there are none
 */
def drawSamples(d: _Distribution, n: Int, seed: Long, offset: Long = 0L): Either[SamplingFailure, Vector[Double]] =
  drawSamples(d, n, seed, offset, Limits.Default)

/** [[drawSamples]] under `limits` (F_0124): at most [[core.Limit.MaxDrawCount]] draws. An
 *  overload rather than a defaulted parameter, which would be a binary break.
 *
 *  @param d      the distribution
 *  @param n      how many draws
 *  @param seed   the seed, any integer
 *  @param offset the position in the stream to start from, `≥ 0`
 *  @param limits the budgets
 *  @return the draws, or why there are none
 */
def drawSamples(d: _Distribution, n: Int, seed: Long, offset: Long, limits: Limits): Either[SamplingFailure, Vector[Double]] =
  val cap = limits.count(Limit.MaxDrawCount)
  if n < 1 || n > cap then Left(SamplingFailure.InvalidCount(n, cap))
  else if offset < 0 then Left(SamplingFailure.InvalidInteger("the offset"))
  else
    val out = new Array[Double](n)
    var i   = 0
    var bad = -1
    while i < n && bad < 0 do
      quantileOf(d, SplitMix64.uniform(seed, offset + i)) match
        case Some(x) => out(i) = x
        case None    => bad = i
      i += 1
    if bad >= 0 then Left(SamplingFailure.Undefined(bad)) else Right(out.toVector)


/** The feedback taps of a maximal-length Fibonacci LFSR, by order: the exponents of a primitive
 *  polynomial `xⁿ + … + 1` over GF(2), the leading one first.  Every entry is checked by running
 *  its full period in `SamplingTest`. */
private val PrbsTaps: Map[Int, List[Int]] = Map(
  2 -> List(2, 1),          3 -> List(3, 2),          4 -> List(4, 3),
  5 -> List(5, 3),          6 -> List(6, 5),          7 -> List(7, 6),
  8 -> List(8, 6, 5, 4),    9 -> List(9, 5),          10 -> List(10, 7),
  11 -> List(11, 9),        12 -> List(12, 11, 10, 4), 13 -> List(13, 12, 11, 8),
  14 -> List(14, 13, 12, 2), 15 -> List(15, 14),        16 -> List(16, 15, 13, 4),
  17 -> List(17, 14),       18 -> List(18, 11),        19 -> List(19, 18, 17, 14),
  20 -> List(20, 17))

/** The output bits of the LFSR of `order` started in `state` (non-zero, below `2^order`), `n` of
 *  them.  A Fibonacci register: the bit shifted out is the output, and the feedback is the XOR of
 *  the tapped bits. */
private[probability] def lfsrBits(order: Int, state: Int, n: Int): Array[Byte] =
  val shifts = PrbsTaps(order).map(order - _).toArray
  val out    = new Array[Byte](n)
  var s      = state
  var k      = 0
  while k < n do
    out(k) = (s & 1).toByte
    var fb = 0
    var j  = 0
    while j < shifts.length do { fb ^= (s >>> shifts(j)) & 1; j += 1 }
    s = (s >>> 1) | (fb << (order - 1))
    k += 1
  out

/** A maximal-length pseudo-random binary sequence of `±1` (F_0067, MPC T3-03): the output of a
 *  linear feedback shift register over a primitive polynomial, `+1` for a set bit.
 *
 *  Its period is `2^order − 1`, and each period holds exactly one more `+1` than `−1` — the
 *  property that makes it a persistently exciting input for identification (F_0060).  The seed
 *  picks the register's starting state, i.e. the **phase** within the period, so two seeds give
 *  rotations of one sequence.  A longer `n` than the period repeats it.
 *
 *  @param order the register length, `MinPrbsOrder … MaxPrbsOrder`
 *  @param n     how many terms, `1 … MaxPrbsLength`
 *  @param seed  the seed, any integer
 *  @return the sequence, or why there is none
 */
def prbsSequence(order: Int, n: Int, seed: Long): Either[SamplingFailure, Vector[Double]] =
  prbsSequence(order, n, seed, Limits.Default)

/** [[prbsSequence]] under `limits` (F_0124): at most [[core.Limit.MaxPrbsLength]] terms. The
 *  order range is not a budget but the verified table, and stays fixed.
 *
 *  @param order  the register length, `MinPrbsOrder … MaxPrbsOrder`
 *  @param n      how many terms
 *  @param seed   the seed, any integer
 *  @param limits the budgets
 *  @return the sequence, or why there is none
 */
def prbsSequence(order: Int, n: Int, seed: Long, limits: Limits): Either[SamplingFailure, Vector[Double]] =
  val cap = limits.count(Limit.MaxPrbsLength)
  if order < MinPrbsOrder || order > MaxPrbsOrder then Left(SamplingFailure.InvalidOrder(order))
  else if n < 1 || n > cap then Left(SamplingFailure.InvalidCount(n, cap))
  else
    val period = (1 << order) - 1
    val start  = 1 + ((SplitMix64.mix(seed) >>> 1) % period).toInt
    Right(lfsrBits(order, start, n).toVector.map(b => if b == 1 then 1.0 else -1.0))


/** A whole number a `Double` holds exactly, read from an evaluated operand. */
private def exactInteger(r: Either[_Expression, _Value]): Option[Long] = r match
  case Right(_Number(d)) if d == math.floor(d) && math.abs(d) <= 9.007199254740992e15 => Some(d.toLong)
  case _                                                                               => None

/** A count, read from an evaluated operand: any number, so a refusal can name it. */
private def countOf(r: Either[_Expression, _Value]): Option[Double] = r match
  case Right(_Number(d)) => Some(d)
  case _                 => None

/** The draws or the sequence as an `n×1` column (Decision A: X-06, vectors are columns). */
private def column(xs: Vector[Double]): _MatrixValue = _MatrixValue(xs.size, 1, xs.toArray)


/** `draw(dist, n, seed[, offset])` — [[drawSamples]] as a node (F_0067): an `n×1` column of draws.
 *  A refusal leaves the call as written, and [[detailed]] carries the reason.
 *
 *  @param dist   the distribution expression
 *  @param n      the count
 *  @param seed   the seed
 *  @param offset the position in the stream, `None` for `0`
 */
case class _Draw(dist: _Expression, n: _Expression, seed: _Expression, offset: Option[_Expression]) extends _Expression:
  override def children: List[_Expression] = dist :: n :: seed :: offset.toList
  override def rebuild(c: List[_Expression]): _Expression = _Draw(c(0), c(1), c(2), c.lift(3))
  override def toString: String = s"draw($dist, $n, $seed${offset.fold("")(o => s", $o")})"

  /** The draws as a column, or why there are none.
   *  @param env the evaluation environment
   *  @return the column, or the reason */
  def detailed(env: Environment): Either[SamplingFailure, _MatrixValue] =
    for
      d <- dist.eval(env) match
             case Right(d: _Distribution) => Right(d)
             case _                       => Left(SamplingFailure.NotADistribution)
      k <- countOf(n.eval(env)).filter(c => c == math.floor(c) && c >= 1 && c <= env.limits(Limit.MaxDrawCount))
             .toRight(SamplingFailure.InvalidCount(countOf(n.eval(env)).getOrElse(Double.NaN), env.limits.count(Limit.MaxDrawCount)))
      s <- exactInteger(seed.eval(env)).toRight(SamplingFailure.InvalidInteger("the seed"))
      o <- offset.fold(Right(0L))(e => exactInteger(e.eval(env)).filter(_ >= 0).toRight(SamplingFailure.InvalidInteger("the offset")))
      xs <- drawSamples(d, k.toInt, s, o, env.limits)
    yield column(xs)

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this), Right(_))


/** `prbs(order, n, seed)` — [[prbsSequence]] as a node (F_0067): an `n×1` column of `±1`.
 *  A refusal leaves the call as written, and [[detailed]] carries the reason.
 *
 *  @param order the register length
 *  @param n     the count
 *  @param seed  the seed
 */
case class _Prbs(order: _Expression, n: _Expression, seed: _Expression) extends _Expression:
  override def children: List[_Expression] = List(order, n, seed)
  override def rebuild(c: List[_Expression]): _Expression = _Prbs(c(0), c(1), c(2))
  override def toString: String = s"prbs($order, $n, $seed)"

  /** The sequence as a column, or why there is none.
   *  @param env the evaluation environment
   *  @return the column, or the reason */
  def detailed(env: Environment): Either[SamplingFailure, _MatrixValue] =
    for
      o <- countOf(order.eval(env)).filter(c => c == math.floor(c) && c >= MinPrbsOrder && c <= MaxPrbsOrder)
             .toRight(SamplingFailure.InvalidOrder(countOf(order.eval(env)).getOrElse(Double.NaN)))
      k <- countOf(n.eval(env)).filter(c => c == math.floor(c) && c >= 1 && c <= env.limits(Limit.MaxPrbsLength))
             .toRight(SamplingFailure.InvalidCount(countOf(n.eval(env)).getOrElse(Double.NaN), env.limits.count(Limit.MaxPrbsLength)))
      s <- exactInteger(seed.eval(env)).toRight(SamplingFailure.InvalidInteger("the seed"))
      xs <- prbsSequence(o.toInt, k.toInt, s, env.limits)
    yield column(xs)

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this), Right(_))
