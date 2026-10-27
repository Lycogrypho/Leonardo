package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

/** F_0238: every figure kind is well-formed XML.  JVM-only, for the JDK's parser. */
class FigureXmlTest extends AnyFlatSpec:

  private def wellFormed(text: String): Boolean =
    val factory = javax.xml.parsers.DocumentBuilderFactory.newInstance()
    factory.newDocumentBuilder().parse(new java.io.ByteArrayInputStream(text.getBytes("UTF-8")))
      .getDocumentElement.getTagName == "svg"

  "every drawing command" should "produce a well-formed SVG document" in {
    val s = new Session()
    for lines <- List("plot x^2 x -1 1", "points x x 0 1 5", "curve [[cos(t)], [sin(t)]] t 0 3",
                      "arrows [[1, -1], [2, 0.5]]", "polygon [[0, 1, 0], [0, 0, 1]]", "hist [[1, 2, 2, 5]]",
                      "bode 1/(s+1) s 0.01 100", "nyquist 1/(s+1) s 0.01 100", "rlocus 1/(s*(s+2)) s 0.1 10",
                      "arrows [[1], [1]] ; curve [[t], [t^2]] t -1 1 ; polygon [[0, 1, 1], [0, 0, 1]]") do
      s.figure(lines) match
        case Right(text) => assert(wellFormed(text), lines)
        case Left(why)   => fail(s"$lines: $why")
  }
