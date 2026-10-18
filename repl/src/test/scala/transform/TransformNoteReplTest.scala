package it.grypho.scala.leonardo
package transform

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


/** The notes a declined transform appends at the prompt (F_0141, F_0144). */
class TransformNoteReplTest extends AnyFlatSpec:

  "fourier" should "say where the pole is when the transform does not converge" in {
    val out = Session().execute("fourier(1, t, w)")
    assert(out.contains("note: fourier declined") && out.contains("imaginary axis"), out)
  }

  it should "answer a convergent transform with no note" in {
    val out = Session().execute("fourier(exp(-abs(t)), t, w)")
    assert(!out.contains("note:") && !out.contains("fourier"), out)
  }

  "fourierCausal" should "name the causal part's pole" in {
    val out = Session().execute("fourierCausal(cos(t), t, w)")
    assert(out.contains("note: fourierCausal declined") && out.contains("causal part"), out)
  }

  "conv" should "say which budget refused it" in {
    val s = Session()
    // 6096 x 6096 = 3.7e7 products, past MaxConvWork; one `ones` stops at MaxDenseDim = 4096.
    s.execute("x := hcat(ones(1, 4096), ones(1, 2000))")
    val out = s.execute("conv(x, x)")
    assert(out.contains("note: conv declined") && out.contains("MaxConvWork"), out)
  }
