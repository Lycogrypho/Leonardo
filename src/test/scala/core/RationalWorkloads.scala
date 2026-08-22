package it.grypho.scala.leonardo
package core

import scala.annotation.tailrec

/** Accumulator for operand-size statistics over a benchmark run.
 *
 *  Deliberately mutable: allocating an immutable statistic per operation would show up in
 *  the very allocation profile the benchmark is trying to measure.
 */
final class BitStats:
  private var count: Long = 0L
  private var total: Long = 0L
  private var peak:  Int  = 0

  /** Records the operand size of one intermediate result.
   *  @param r the value produced by the step being measured
   */
  def record(r: _Rational): Unit =
    val b = r.maxBitLength
    count += 1
    total += b
    if b > peak then peak = b

  /** Largest `max(numerator, denominator)` bit length seen. */
  def max: Int = peak

  /** Mean `max(numerator, denominator)` bit length over all recorded steps. */
  def mean: Double = if count == 0L then 0.0 else total.toDouble / count.toDouble


/** The three workloads of the gcd-policy spike (issue 4.M).
 *
 *  Chosen so the policies actually diverge rather than to be representative of typical use:
 *  `gcd` cost and operand growth pull in opposite directions, so the set needs both a
 *  cheap-arithmetic case (the primitive chain) and a growth-heavy one (Gaussian
 *  elimination, the classic denominator blow-up and the reason "fraction-free" elimination
 *  exists as a named technique).
 *
 *  Every workload takes the same two knobs — the [[GcdPolicy]] under test and the
 *  re-approximation precision — because the hypothesis being tested is about their
 *  *interaction*: re-approximation caps operand size for free, so a single-precision
 *  comparison of two policies would very likely mislead.
 *
 *  Shared by [[RationalBenchmark]] (which times them) and `RationalTest` (which asserts all
 *  policies agree on their results).  That second use is why they live here rather than
 *  inside the benchmark: the cross-policy equality check is a genuine correctness test of
 *  the [[GcdPolicy.Lazy]] path, and outlives the decision.
 */
object RationalWorkloads:

  /** Convenience exact constructor; the denominator is known non-zero at every call site. */
  private def rat(n: Int, d: Int, policy: GcdPolicy): _Rational =
    _Rational.make(BigInt(n), BigInt(d), policy)

  /** Records the operand the arithmetic actually built, then applies the working-precision
   *  cap (or leaves the value exact when there is none).
   *
   *  The order matters, and getting it wrong silently ruins the experiment: `approximate`
   *  reduces internally and returns a convergent in lowest terms, so a *post*-cap
   *  measurement is identical under every policy by construction.  The size that explains
   *  the wall-clock is the pre-cap one — the numerator and denominator the `BigInt`
   *  multiply was handed.
   */
  private def step(r: _Rational, digits: Option[Int], stats: BitStats): _Rational =
    stats.record(r)
    digits.fold(r)(r.approximateToDigits)

  /** A long chain of the three primitives — `acc = (acc + 1/k) · k/(k+1)` for `k = 1..n`.
   *
   *  Cheap per operation and free of operand-on-operand growth (one side is always small),
   *  so this is where a `gcd` per step should cost the most relative to the work done.
   *
   *  @param n       number of steps
   *  @param policy  the reduction policy under test
   *  @param digits  re-approximation precision in decimal digits; `None` stays exact
   *  @param stats   collector for operand sizes
   *  @return the final accumulator
   */
  def primitiveChain(n: Int, policy: GcdPolicy, digits: Option[Int], stats: BitStats): _Rational =
    (1 to n).foldLeft(_Rational.Zero) { (acc, k) =>
      step(acc.add(rat(1, k, policy), policy).multiply(rat(k, k + 1, policy), policy), digits, stats)
    }

  /** The `n × n` Hilbert system `H·x = 1`, solved by Gaussian elimination with partial
   *  pivoting.
   *
   *  `H(i,j) = 1/(i+j+1)` is exactly representable as a rational and famously
   *  ill-conditioned, which makes it the honest worst case here: entries combine with each
   *  other at every elimination step, so operand size doubles with *depth* — and depth is
   *  `n`, not the `n³` operation count.
   *
   *  @param n       the system size
   *  @param policy  the reduction policy under test
   *  @param digits  re-approximation precision in decimal digits; `None` stays exact
   *  @param stats   collector for operand sizes
   *  @return the solution vector, or `None` if a pivot vanishes
   */
  def hilbertSolve(n: Int, policy: GcdPolicy, digits: Option[Int], stats: BitStats): Option[Vector[_Rational]] =
    // Augmented n x (n+1): Hilbert matrix beside a right-hand side of ones.
    val augmented = Vector.tabulate(n, n + 1) { (i, j) =>
      if j == n then _Rational.One else rat(1, i + j + 1, policy)
    }

    def scaleSubtract(target: Vector[_Rational], pivot: Vector[_Rational],
                      factor: _Rational, from: Int): Vector[_Rational] =
      target.zipWithIndex.map { (cell, j) =>
        if j < from then cell
        else step(cell.subtract(factor.multiply(pivot(j), policy), policy), digits, stats)
      }

    @tailrec
    def forward(m: Vector[Vector[_Rational]], k: Int): Option[Vector[Vector[_Rational]]] =
      if k >= n then Some(m)
      else
        val best = (k until n).foldLeft(k)((b, i) => if m(i)(k).abs.compare(m(b)(k).abs) > 0 then i else b)
        val swapped = if best == k then m else m.updated(k, m(best)).updated(best, m(k))
        val pivot   = swapped(k)
        if pivot(k).isZero then None
        else
          val reduced = swapped.zipWithIndex.map { (row, i) =>
            if i <= k then row
            else
              row(k).divide(pivot(k), policy) match
                case None         => row
                case Some(factor) => scaleSubtract(row, pivot, factor, k)
          }
          forward(reduced, k + 1)

    // Back substitution: x_i = (rhs_i - sum_{j>i} a_ij x_j) / a_ii, filled bottom-up.
    def backSubstitute(m: Vector[Vector[_Rational]]): Option[Vector[_Rational]] =
      (n - 1 to 0 by -1).foldLeft(Option(Vector.empty[_Rational])) { (acc, i) =>
        acc.flatMap { solved =>
          // `solved` holds x_{i+1}..x_{n-1} in order, so index j maps to solved(j - i - 1).
          val rhs = (i + 1 until n).foldLeft(m(i)(n)) { (sum, j) =>
            step(sum.subtract(m(i)(j).multiply(solved(j - i - 1), policy), policy), digits, stats)
          }
          rhs.divide(m(i)(i), policy).map(x => step(x, digits, stats) +: solved)
        }
      }

    forward(augmented, 0).flatMap(backSubstitute)

  /** The exponential series `Σ x^k / k!` truncated after `terms` terms.
   *
   *  Deep, predictable denominator growth: each step multiplies by `x` and divides by `k`,
   *  so under [[GcdPolicy.Lazy]] the denominator is a factorial rather than a doubling —
   *  a different growth law from the elimination case, which is the point of including it.
   *
   *  @param terms   number of terms, `k = 0 .. terms-1`
   *  @param x       the point to evaluate at
   *  @param policy  the reduction policy under test
   *  @param digits  re-approximation precision in decimal digits; `None` stays exact
   *  @param stats   collector for operand sizes
   *  @return the partial sum
   */
  def expSeries(terms: Int, x: _Rational, policy: GcdPolicy,
                digits: Option[Int], stats: BitStats): _Rational =
    @tailrec
    def go(k: Int, term: _Rational, sum: _Rational): _Rational =
      if k >= terms then sum
      else
        val next = step(term.multiply(x, policy).multiply(rat(1, k, policy), policy), digits, stats)
        val acc  = step(sum.add(next, policy), digits, stats)
        go(k + 1, next, acc)

    go(1, _Rational.One, _Rational.One)
