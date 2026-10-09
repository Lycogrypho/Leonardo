package it.grypho.scala.leonardo
package control

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0060 at the prompt: the model binds by tuple assignment, converts to a state-space row,
 *  and a refusal says why. The data is `y[k] = 0.5·y[k−1] + u[k−1]` with integer inputs, so
 *  every sample is an exact short decimal and prints the same on every platform. */
class ArxReplTest extends AnyFlatSpec:

  private def session(): Session =
    val s = Session()
    s.execute("u := [[1, 0, 2, -1, 3, 1, -2, 1, 0, 2]]")
    s.execute("y := [[0, 1, 0.5, 2.25, 0.125, 3.0625, 2.53125, -0.734375, 0.6328125, 0.31640625]]")
    s

  "the REPL" should "bind a, b := arx(...) and realise the model with arx2ss" in
  {
    val s = session()
    s.execute("a, b := arx(u, y, 1, 1, 1)")
    assert(s.execute("a") == s.execute("[[0.5]]"))
    assert(s.execute("b") == s.execute("[[1]]"))
    s.execute("A, B, C, D := arx2ss(a, b, 1)")
    assert(s.execute("A") == s.execute("[[0.5]]"))
    assert(s.execute("D") == s.execute("[[0]]"))
  }

  it should "say why an identification was declined" in
  {
    val s = session()
    s.execute("uc := [[1, 1, 1, 1, 1, 1, 1, 1]]")
    s.execute("yc := [[0, 1, 1.5, 1.75, 1.875, 1.9375, 1.96875, 1.984375]]")
    val flat = s.execute("arx(uc, yc, 1, 2, 1)")
    assert(flat.contains("note: arx declined:") && flat.contains("excite"), flat)
    val short = s.execute("arx(u, y, 5, 5, 1)")
    assert(short.contains("note: arx declined:") && short.contains("too short"), short)
    assert(s.execute("arx2ss(0, [[3]], 0)").contains("note: arx2ss declined:"))
  }
