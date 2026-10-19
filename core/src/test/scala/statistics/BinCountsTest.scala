package it.grypho.scala.leonardo
package statistics

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** `bincounts(x, lo, hi, k)` (F_0135): the counts of a sample in `k` equal bins over `[lo, hi]`,
 *  half-open except the last, which is closed so the maximum is counted. */
class BinCountsTest extends AnyFlatSpec:

  private val env = new Environment()
  private def ev(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)

  "bincounts" should "count each value in its half-open bin, the last bin closed" in {
    assert(ev("bincounts([[1, 2, 2, 3, 4, 5]], 1, 5, 4)") == ev("[[1], [2], [1], [2]]"))
  }

  it should "leave out values outside [lo, hi]" in {
    assert(ev("bincounts([[0, 1, 2, 9]], 1, 3, 2)") == ev("[[1], [1]]"))
  }

  it should "agree with the library kernel" in {
    assert(binCounts(Vector(0.5, 0.5, 2.5), 0.0, 3.0, 3).contains(Vector(2, 0, 1)))
  }

  it should "bin a span too wide for one Double without putting everything in the first bin (F_0150)" in {
    // hi - lo = 3e308 overflows to Inf: every value used to land in bin 0.
    assert(binCounts(Vector(-1e308, 0.0, 1e308), -1.5e308, 1.5e308, 3).contains(Vector(1, 1, 1)))
  }

  it should "read an exact sample" in {
    assert(Parser.parse("bincounts([[1/2, 3/2]], 0, 2, 2)", Some(30)).get.eval(env) == ev("[[1], [1]]"))
  }

  it should "stay unevaluated for lo >= hi, a k that is not a positive whole number, or a non-sample" in {
    for s <- List("bincounts([[1, 2]], 3, 3, 2)", "bincounts([[1, 2]], 0, 3, 0)", "bincounts([[1, 2]], 0, 3, 1.5)",
                  "bincounts(x, 0, 3, 2)") do
      assert(ev(s).isLeft, s)
  }

  it should "round-trip and reserve its word" in {
    val e = Parser.parse("bincounts(x, 0, 1, 10)").get
    assert(Parser.parse(e.toString).get == e)
    assert(Parser.ReservedWords.contains("bincounts") && !Parser.ReservedWords.contains("hist"))
  }
