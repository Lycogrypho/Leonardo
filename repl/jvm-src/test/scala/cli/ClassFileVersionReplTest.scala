package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

/** The REPL module is compiled for the same JDK floor as the library (F_0126 Decision A). */
class ClassFileVersionReplTest extends AnyFlatSpec:

  private def majorOf(c: Class[?]): Int =
    val in = c.getResourceAsStream("/" + c.getName.replace('.', '/') + ".class")
    try
      val b = in.readNBytes(8)
      ((b(6) & 0xff) << 8) | (b(7) & 0xff)
    finally in.close()

  "the REPL" should "be compiled for the JDK 25 floor (class-file major 69)" in {
    assert(majorOf(classOf[Session]) == 69)
  }
