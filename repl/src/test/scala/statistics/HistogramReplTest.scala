package it.grypho.scala.leonardo
package statistics

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


/** `Session.histogramBins` (F_0135): the data behind the browser's `hist` command. */
class HistogramReplTest extends AnyFlatSpec:

  "histogramBins" should "bin a bound sample over its own range in k bins" in {
    val s = Session()
    s.execute("x := [[1, 2, 2, 3, 4, 5]]")
    s.histogramBins("x 4") match
      case Right(h) =>
        assert(h.edges == Vector(1.0, 2.0, 3.0, 4.0, 5.0))
        assert(h.counts == Vector(1, 2, 1, 2))
      case Left(why) => fail(why)
  }

  it should "choose Sturges' number of bins when none is given" in {
    // ceil(log2 6) + 1 = 4
    Session().histogramBins("[[1, 2, 2, 3, 4, 5]]").map(_.counts.size) match
      case Right(k)  => assert(k == 4)
      case Left(why) => fail(why)
  }

  it should "give a constant sample one unit-wide range around its value" in {
    Session().histogramBins("[[2, 2, 2]] 1") match
      case Right(h)  => assert(h.edges == Vector(1.5, 2.5) && h.counts == Vector(3))
      case Left(why) => fail(why)
  }

  it should "read a trailing integer as part of the expression when the rest is not a sample (F_0143)" in {
    // `y * 2` used to split into the sample `y *` with 2 bins and fail with a parse error that
    // never said the 2 had been taken as the count.
    val s = Session()
    s.execute("y := [[1, 2, 3]]")
    s.histogramBins("y * 2") match
      case Right(h)  => assert(h.edges.head == 2.0 && h.edges.last == 6.0 && h.counts.sum == 3, h)
      case Left(why) => fail(why)
    // a sample before the integer keeps the documented reading: the integer is the count
    assert(s.histogramBins("y 2").map(_.counts.size) == Right(2))
  }

  it should "refuse a sample with a cell that is not a real number, rather than drop it (F_0143)" in {
    // [[1, 2, 3+2i]] was drawn as a histogram of two values.
    assert(Session().histogramBins("[[1, 2, 3 + 2i]]").left.exists(_.contains("real")))
    assert(Session().histogramBins("[[1, true, 3]]").isLeft)
  }

  it should "refuse a bin count it cannot read, rather than fall back to Sturges' rule (F_0143)" in {
    Session().histogramBins("[[1, 2, 3]] 99999999999") match
      case Left(why) => assert(why.contains("99999999999"), why)
      case Right(h)  => fail(s"drew ${h.counts.size} bins")
  }

  it should "explain itself when there is no sample or no argument" in {
    assert(Session().histogramBins("").left.exists(_.startsWith("usage:")))
    assert(Session().histogramBins("y").isLeft)
    assert(Session().histogramBins("[[1, 2]] 0").isLeft)
  }
