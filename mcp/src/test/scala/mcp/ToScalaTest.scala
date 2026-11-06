package it.grypho.scala.leonardo
package mcp

import core.*
import org.scalatest.flatspec.AnyFlatSpec

/** [[ToScala]]'s round trip (F_0126 Decision D): `GeneratedRoundTrip` holds every cheat-sheet case
 *  rendered as Scala source, compiled with this suite — so each rendering is valid Scala — and each
 *  must equal the value recomputed from its line. */
class ToScalaTest extends AnyFlatSpec:

  "every rendered case" should "compile to a value equal to the original" in {
    for (line, exact, evaluated, value) <- GeneratedRoundTrip.cases do
      val expected = ScalaRoundTrip.valueOf(line, exact, evaluated)
      assert(expected.contains(value), s"$line (exact = $exact, evaluated = $evaluated)")
  }

  "the corpus" should "be large, and almost all of it rendered" in {
    assert(GeneratedRoundTrip.total > 1000, GeneratedRoundTrip.total)
    assert(GeneratedRoundTrip.cases.size >= 0.95 * GeneratedRoundTrip.total,
      GeneratedRoundTrip.declined.mkString("\n"))
  }

  "ToScala" should "write the values with private constructors through their factories" in {
    val third = _Rational.of(BigInt(1), BigInt(3)).getOrElse(fail("1/3"))
    assert(ToScala(third) == Right("core._Rational.of(BigInt(1), BigInt(3)).get"))
    assert(ToScala(_Complex.of(1.0, -2.0)) == Right("core._Complex.of(1.0, -2.0)"))
    assert(ToScala(_Variable("x")) == Right("core._Variable(\"x\")"))
  }

  it should "decline what is not the library's" in {
    assert(ToScala(new Object).isLeft)
    assert(ToScala(java.time.Duration.ZERO).isLeft)
  }
