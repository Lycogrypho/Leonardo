package it.grypho.scala.leonardo
package core

import org.scalatest.flatspec.AnyFlatSpec

/** The JDK floor is what the class files say (F_0126 Decision A): a raised or lowered floor is a
 *  change every consumer feels and MiMa cannot see, so the emitted version is pinned here. */
class ClassFileVersionTest extends AnyFlatSpec:

  /** The class-file major version of a compiled class, read off its own bytes. */
  private def majorOf(c: Class[?]): Int =
    val in = c.getResourceAsStream("/" + c.getName.replace('.', '/') + ".class")
    try
      val b = in.readNBytes(8)
      ((b(6) & 0xff) << 8) | (b(7) & 0xff)
    finally in.close()

  "the library" should "be compiled for the JDK 25 floor (class-file major 69)" in {
    for c <- Seq(classOf[_Number], classOf[Environment], classOf[scalar.Sum], classOf[parser.Parser.type]) do
      assert(majorOf(c) == 69, c.getName)
  }
