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

  it should "explain itself when there is no sample or no argument" in {
    assert(Session().histogramBins("").left.exists(_.startsWith("usage:")))
    assert(Session().histogramBins("y").isLeft)
    assert(Session().histogramBins("[[1, 2]] 0").isLeft)
  }
