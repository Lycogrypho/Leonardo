package it.grypho.scala.leonardo
package scalar

import core.*
import scalar.*
import parser.Parser
import cli.Session
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
    // x values: -1.0, 0.0, 1.0 â€” the middle point is 1/0 = Infinity, so dropped
    val pts = sample(Ratio(_Number(1), x), x, -1.0, 1.0, 3, env)
    assert(pts.length == 2)
    assert(pts.forall { case (_, y) => !y.isNaN && !y.isInfinite })
  }

  "sample of a complex-only result" should "return empty (complex not _Number)" in
  {
    // log(-|x|) for x in [1, 2] gives complex results â†’ not _Number â†’ all dropped
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

  // --- REPL integration ---

  "the samples command" should "return n tab-separated x/y lines" in
  {
    val s   = Session()
    val out = s.execute("samples x*x x 0 1 5")
    val lines = out.linesIterator.toList
    assert(lines.length == 5)
    lines.foreach { line =>
      val cols = line.split("\t")
      assert(cols.length == 2, s"expected two tab-separated columns in: $line")
    }
  }

  "the samples command" should "respect the optional n argument" in
  {
    val s = Session()
    assert(s.execute("samples x x 0 1 10").linesIterator.length == 10)
  }

  "the samples command" should "default to 200 points" in
  {
    val s = Session()
    assert(s.execute("samples x x 0 1").linesIterator.length == 200)
  }

  "the samples command" should "accept a defined function name as the expression" in
  {
    val s = Session()
    s.execute("f := x * x")
    val out = s.execute("samples f x 0 1 3")
    assert(out.linesIterator.length == 3)
  }

  "the samples command" should "report (no finite values) when all points are filtered" in
  {
    val s = Session()
    // log(-x) for x in [1,2] â†’ complex â†’ filtered
    val out = s.execute("samples log(0 - x) x 1 2 3")
    assert(out.contains("no finite values"))
  }

  "the samples command with lo >= hi" should "report an error" in
  {
    val s = Session()
    assert(s.execute("samples x x 5 1").contains("lo must be"))
  }

  "the samples command with too few arguments" should "show usage" in
  {
    val s = Session()
    assert(s.execute("samples x x").contains("usage"))
  }

  "the samples command with underscore variable name" should "accept x_1 and sample the expression" in
  {
    val s = Session()
    // x_1 is a valid variable name (grammar extended to [a-zA-Z][a-zA-Z0-9_]*); the command
    // should produce data rows, not a usage error.
    val out = s.execute("samples x_1*x_1 x_1 0 1 5")
    assert(!out.contains("usage"), s"expected data rows, got: $out")
    assert(out.linesIterator.length == 5)
  }

  "the samples command with underscore variable name in parser" should "produce a valid parse" in
  {
    import parser.Parser
    val r = Parser.parse("x_1 + alpha_hat")
    assert(r.successful, s"expected successful parse, got: $r")
  }

  "assigning to an underscore variable name" should "be accepted by the REPL" in
  {
    val s = Session()
    assert(s.execute("f_1 := 2").contains("f_1"))
    assert(s.execute("f_1") == "2.0")
  }

  "samples as a variable name" should "be rejected by the parser" in
  {
    val r = Parser.parse("samples")
    assert(!r.successful)
  }

  "samples := 3" should "be rejected as a reserved word" in
  {
    val s = Session()
    assert(s.execute("samples := 3").contains("reserved word"))
  }

  "help samples" should "return topic-specific help" in
  {
    assert(Session().execute("help samples").contains("samples"))
  }
