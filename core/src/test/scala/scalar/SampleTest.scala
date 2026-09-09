package it.grypho.scala.leonardo
package scalar

import core.*
import scalar.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class SampleTest extends AnyFlatSpec:

  val env = new Environment()
  val x   = _Variable("x")

  // --- core sample function ---

  "sample of x^2" should "return n points in ascending x order with correct y values" in
  {
    val pts = sample(Power(x, _Number(2)), x, 0.0, 1.0, 5, env)
    assert(pts.length == 5)
    assert(pts.map(_._1) == Vector(0.0, 0.25, 0.5, 0.75, 1.0))
    pts.foreach { (xi, yi) => assert(math.abs(yi - xi * xi) < 1e-12) }
    pts.zip(pts.tail).foreach { case ((x1, _), (x2, _)) => assert(x1 < x2) }
  }

  "sample" should "skip non-finite results (e.g., 1/x at x=0)" in
  {
    // x values: -1.0, 0.0, 1.0 — the middle point is 1/0 = Infinity, so dropped
    val pts = sample(Ratio(_Number(1), x), x, -1.0, 1.0, 3, env)
    assert(pts.length == 2)
    assert(pts.forall { case (_, y) => !y.isNaN && !y.isInfinite })
  }

  "sample of a complex-only result" should "return empty (complex not _Number)" in
  {
    // log(-|x|) for x in [1, 2] gives complex results → not _Number → all dropped
    val pts = sample(Ln(Product(_Number(-1), x)), x, 1.0, 2.0, 5, env)
    assert(pts.isEmpty)
  }

  "sample" should "use the fallback eval path for non-compilable expressions" in
  {
    // _Derivative is not compilable; eval computes 2*x symbolically then numerically
    val d   = _Derivative(Power(x, _Number(2)), x)
    val pts = sample(d, x, 0.0, 1.0, 3, env)
    assert(pts.length == 3)
    assert(math.abs(pts(0)._2 - 0.0) < 1e-9)   // d/dx x^2 at x=0 = 0
    assert(math.abs(pts(1)._2 - 1.0) < 1e-9)   // at x=0.5 = 1
    assert(math.abs(pts(2)._2 - 2.0) < 1e-9)   // at x=1 = 2
  }

  "sample with n=1" should "return a single point at lo" in
  {
    val pts = sample(x, x, 3.0, 7.0, 1, env)
    assert(pts.length == 1)
    assert(pts.head == (3.0, 3.0))
  }

  "sample" should "resolve other free variables from env" in
  {
    val a   = _Variable("a")
    val e2  = new Environment(5, Map("a" -> _Number(2.0)))
    // f(x) = a * x = 2 * x, sampled at x=1,2,3
    val pts = sample(Product(a, x), x, 1.0, 3.0, 3, e2)
    assert(pts.length == 3)
    assert(math.abs(pts(0)._2 - 2.0) < 1e-12)
    assert(math.abs(pts(2)._2 - 6.0) < 1e-12)
  }

  // --- Syntax extension ---

  "Syntax.sample" should "produce the same result as the package function" in
  {
    import Syntax.*
    val e            = Power(x, _Number(2))
    val viaExtension = e.sample(x, 0.0, 1.0, 5, env)
    val viaFunction  = scalar.sample(e, x, 0.0, 1.0, 5, env)
    assert(viaExtension == viaFunction)
  }

  // The `samples` COMMAND is a REPL feature; its tests live in scalar/SampleReplTest.scala
  // (issue 5.2 phase 1.1). What stays here is the grammar's side of the same behaviour, which
  // needs no session.

  "the samples command with underscore variable name in parser" should "produce a valid parse" in
  {
    import parser.Parser
    val r = Parser.parse("x_1 + alpha_hat")
    assert(r.successful, s"expected successful parse, got: $r")
  }

  "samples as a variable name" should "be rejected by the parser" in
  {
    val r = Parser.parse("samples")
    assert(!r.successful)
  }
