package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class SpecialFunctionsTest extends AnyFlatSpec:

  val x = _Variable("x")
  val env = new Environment()

  def parse(input: String): _Expression =
    val result = Parser.parse(input)
    assert(result.successful, s"parse failed for \"$input\": $result")
    result.get

  /** The number an expression evaluates to, failing otherwise. */
  def num(e: _Expression): Double =
    e.eval(env) match
      case Right(_Number(d)) => d
      case other             => fail(s"expected a number but got: $other")

  def assertClose(actual: Double, expected: Double, tol: Double = 1e-9, clue: String = ""): Unit =
    assert(math.abs(actual - expected) <= tol * math.max(1.0, math.abs(expected)),
      s"$clue expected $expected but got $actual")

  // --- factorial: the exact integer path ---

  "factorialOf" should "be exact on the small integers" in
  {
    val expected = List(1.0, 1.0, 2.0, 6.0, 24.0, 120.0, 720.0, 5040.0, 40320.0, 362880.0, 3628800.0)
    for (v, n) <- expected.zipWithIndex do
      assert(factorialOf(n.toDouble).contains(v), s"$n! should be $v")
  }

  it should "match the recurrence n! = n * (n-1)! up to 20" in
  {
    for n <- 1 to 20 do
      (factorialOf(n), factorialOf(n - 1)) match
        case (Some(a), Some(b)) => assertClose(a, n * b, 1e-12, s"$n!:")
        case other              => fail(s"undefined at $n: $other")
  }

  it should "be finite at the overflow bound and symbolic past it" in
  {
    assert(factorialOf(MaxFactorial).exists(d => !d.isInfinite), "170! must be finite")
    assert(factorialOf(MaxFactorial + 1).isEmpty, "171! overflows and must stay symbolic")
    assert(factorialOf(1e6).isEmpty)
  }

  it should "be undefined at the negative integers (the gamma poles)" in
  {
    for n <- List(-1.0, -2.0, -10.0) do
      assert(factorialOf(n).isEmpty, s"$n! must be undefined")
  }

  it should "continue analytically at non-integer arguments" in
  {
    // fact(1/2) = Gamma(3/2) = sqrt(pi)/2
    factorialOf(0.5) match
      case Some(d) => assertClose(d, math.sqrt(math.Pi) / 2.0, 1e-9, "fact(0.5):")
      case None    => fail("fact(0.5) should be defined via Gamma")
    // negative NON-integers are fine: fact(-0.5) = Gamma(0.5) = sqrt(pi)
    factorialOf(-0.5) match
      case Some(d) => assertClose(d, math.sqrt(math.Pi), 1e-9, "fact(-0.5):")
      case None    => fail("fact(-0.5) should be defined")
  }

  it should "reject non-finite input" in
  {
    assert(factorialOf(Double.NaN).isEmpty)
    assert(factorialOf(Double.PositiveInfinity).isEmpty)
  }

  // --- multifactorial ---

  "multiFactorialOf" should "reduce to the ordinary factorial at k = 1" in
  {
    for n <- 0 to 20 do
      assert(multiFactorialOf(n, 1) == factorialOf(n), s"mfact($n, 1) must equal $n!")
  }

  it should "give the standard double factorials at k = 2" in
  {
    // 5!! = 15, 6!! = 48, 7!! = 105, 8!! = 384
    assert(multiFactorialOf(5, 2).contains(15.0))
    assert(multiFactorialOf(6, 2).contains(48.0))
    assert(multiFactorialOf(7, 2).contains(105.0))
    assert(multiFactorialOf(8, 2).contains(384.0))
    assert(multiFactorialOf(0, 2).contains(1.0))
    assert(multiFactorialOf(1, 2).contains(1.0))
  }

  it should "step by k for larger k" in
  {
    assert(multiFactorialOf(10, 3).contains(10.0 * 7 * 4 * 1))
    assert(multiFactorialOf(11, 4).contains(11.0 * 7 * 3))
  }

  it should "stay undefined for a negative, non-integer, or non-positive step" in
  {
    assert(multiFactorialOf(-1, 2).isEmpty)
    assert(multiFactorialOf(2.5, 2).isEmpty)
    assert(multiFactorialOf(5, 0).isEmpty)
    assert(multiFactorialOf(5, -1).isEmpty)
    assert(multiFactorialOf(5, 1.5).isEmpty)
  }

  // --- gamma ---

  "gammaOf" should "reproduce the factorial: Gamma(n) = (n-1)!" in
  {
    for n <- 1 to 20 do
      (gammaOf(n), factorialOf(n - 1)) match
        case (Some(g), Some(f)) => assertClose(g, f, 1e-10, s"Gamma($n):")
        case other              => fail(s"undefined at $n: $other")
  }

  it should "give sqrt(pi) at one half" in
  {
    gammaOf(0.5) match
      case Some(d) => assertClose(d, math.sqrt(math.Pi), 1e-12, "Gamma(0.5):")
      case None    => fail("Gamma(0.5) must be defined")
  }

  it should "satisfy the recurrence Gamma(z+1) = z * Gamma(z)" in
  {
    for z <- List(0.25, 0.5, 1.3, 2.7, 5.5, 12.4) do
      (gammaOf(z + 1), gammaOf(z)) match
        case (Some(a), Some(b)) => assertClose(a, z * b, 1e-9, s"Gamma($z + 1):")
        case other              => fail(s"undefined at $z: $other")
  }

  it should "satisfy the reflection formula for negative arguments" in
  {
    // Gamma(z)Gamma(1-z) = pi / sin(pi z)
    for z <- List(-0.5, -1.5, -2.3, 0.3) do
      (gammaOf(z), gammaOf(1 - z)) match
        case (Some(a), Some(b)) =>
          assertClose(a * b, math.Pi / math.sin(math.Pi * z), 1e-8, s"reflection at $z:")
        case other => fail(s"undefined at $z: $other")
  }

  it should "be undefined at the poles" in
  {
    for z <- List(0.0, -1.0, -2.0, -3.0, -50.0) do
      assert(gammaOf(z).isEmpty, s"Gamma($z) is a pole and must be undefined")
  }

  it should "stay symbolic where it overflows" in
  {
    assert(gammaOf(1e5).isEmpty, "Gamma(1e5) overflows a Double")
  }

  // --- log-gamma ---

  "lgammaOf" should "agree with log(gamma) where gamma is finite" in
  {
    for z <- List(0.5, 1.0, 2.0, 7.5, 20.0, 100.0) do
      (lgammaOf(z), gammaOf(z)) match
        case (Some(lg), Some(g)) => assertClose(lg, math.log(g), 1e-9, s"lgamma($z):")
        case other               => fail(s"undefined at $z: $other")
  }

  it should "stay finite far past the point where gamma overflows" in
  {
    assert(gammaOf(1e5).isEmpty, "precondition: gamma overflows here")
    lgammaOf(1e5) match
      case Some(d) => assert(d > 0 && !d.isInfinite, s"lgamma(1e5) should be finite, got $d")
      case None    => fail("lgamma must survive where gamma overflows -- that is its purpose")
  }

  it should "be undefined at the poles" in
  {
    for z <- List(0.0, -1.0, -5.0) do assert(lgammaOf(z).isEmpty)
  }

  // --- beta ---

  "betaOf" should "be symmetric" in
  {
    for (a, b) <- List((1.0, 2.0), (2.5, 3.5), (0.5, 0.5), (10.0, 3.0)) do
      (betaOf(a, b), betaOf(b, a)) match
        case (Some(p), Some(q)) => assertClose(p, q, 1e-10, s"B($a,$b) symmetry:")
        case other              => fail(s"undefined at ($a,$b): $other")
  }

  it should "match the gamma quotient" in
  {
    for (a, b) <- List((2.0, 3.0), (1.5, 2.5), (4.0, 0.5)) do
      (betaOf(a, b), gammaOf(a), gammaOf(b), gammaOf(a + b)) match
        case (Some(bt), Some(ga), Some(gb), Some(gab)) =>
          assertClose(bt, ga * gb / gab, 1e-9, s"B($a,$b):")
        case other => fail(s"undefined at ($a,$b): $other")
  }

  it should "give the exact small values" in
  {
    // B(1,1) = 1 ; B(1,n) = 1/n ; B(0.5,0.5) = pi
    assertClose(betaOf(1, 1).getOrElse(fail("B(1,1)")), 1.0, 1e-12)
    assertClose(betaOf(1, 4).getOrElse(fail("B(1,4)")), 0.25, 1e-10)
    assertClose(betaOf(0.5, 0.5).getOrElse(fail("B(.5,.5)")), math.Pi, 1e-10)
  }

  it should "survive large arguments through the log path" in
  {
    betaOf(500.0, 500.0) match
      case Some(d) => assert(d > 0.0 && !d.isInfinite && !d.isNaN, s"got $d")
      case None    => fail("B(500,500) should be representable via lgamma")
  }

  // --- AST nodes and evaluation ---

  "the factorial node" should "evaluate and stay symbolic out of domain" in
  {
    assertClose(num(parse("fact(5)")), 120.0)
    assertClose(num(parse("fact(0)")), 1.0)
    assert(parse("fact(-1)").eval(env).isLeft, "fact(-1) is a pole")
    assert(parse("fact(x)").eval(env).isLeft, "free variable stays symbolic")
  }

  "the multifactorial nodes" should "evaluate, with dfact sugar for step 2" in
  {
    assertClose(num(parse("mfact(7, 2)")), 105.0)
    assertClose(num(parse("dfact(7)")), 105.0)
    assertClose(num(parse("mfact(6, 1)")), 720.0)
    // dfact desugars, exactly as log(x) desugars to LogBase(x, 10)
    assert(parse("dfact(7)") == MultiFactorial(_Number(7), _Number(2)))
  }

  "the Gamma node" should "evaluate and stay symbolic at the poles" in
  {
    assertClose(num(parse("Gamma(5)")), 24.0)
    assertClose(num(parse("Gamma(0.5)")), math.sqrt(math.Pi), 1e-9)
    assert(parse("Gamma(0)").eval(env).isLeft, "Gamma(0) is a pole")
    assert(parse("Gamma(-2)").eval(env).isLeft)
  }

  "the lgamma and Beta nodes" should "evaluate" in
  {
    assertClose(num(parse("lgamma(5)")), math.log(24.0), 1e-9)
    assertClose(num(parse("Beta(1, 4)")), 0.25, 1e-9)
    assert(parse("Beta(0, 1)").eval(env).isLeft, "Beta at a pole stays symbolic")
  }

  "the special functions" should "distribute element-wise over a dense matrix" in
  {
    parse("fact([[0, 1], [2, 3]])").eval(env) match
      case Right(m: _MatrixValue) =>
        assert(m.toVector == Vector(1.0, 1.0, 2.0, 6.0), s"got ${m.toVector}")
      case other => fail(s"expected a dense matrix, got: $other")
  }

  it should "leave the whole matrix symbolic when an element is out of domain" in
  {
    assert(parse("fact([[1, -1]])").eval(env).isLeft)
  }

  // --- naming: the capitalisation decision ---

  "lowercase gamma and beta" should "remain usable as ordinary variables" in
  {
    assert(parse("gamma") == _Variable("gamma"))
    assert(parse("alpha + beta") == Sum(_Variable("alpha"), _Variable("beta")))
    assert(parse("gamma * beta") == Product(_Variable("gamma"), _Variable("beta")))
  }

  "the capitalised names" should "be reserved words" in
  {
    for w <- List("Gamma", "Beta", "fact", "dfact", "mfact", "lgamma") do
      assert(Parser.ReservedWords.contains(w), s"'$w' must be reserved")
    assert(!Parser.parse("Gamma").successful, "bare Gamma is a parse error")
    assert(!Parser.parse("Beta").successful)
  }

  it should "not reserve the lowercase spellings" in
  {
    for w <- List("gamma", "beta") do
      assert(!Parser.ReservedWords.contains(w), s"'$w' must stay available as a variable")
  }

  // --- derivative: needs digamma, so it stays symbolic rather than being wrong ---

  "differentiating gamma or factorial" should "stay symbolic (digamma is not implemented)" in
  {
    assert(derive(parse("Gamma(x)"), x).isInstanceOf[_Derivative])
    assert(derive(parse("fact(x)"), x).isInstanceOf[_Derivative])
  }

  // --- round-trip ---

  "the special functions" should "round-trip through toString" in
  {
    val cases = List[_Expression](
      Factorial(x), MultiFactorial(x, _Number(3)), Gamma(x), LogGamma(x), Beta(x, _Number(2)),
      Factorial(Sum(x, _Number(1))), Gamma(Product(_Number(2), x)))
    for e <- cases do
      val printed  = e.toString
      val reparsed = Parser.parse(printed)
      assert(reparsed.successful, s"toString did not re-parse: \"$printed\" ($reparsed)")
      assert(reparsed.get == e, s"round-trip changed \"$printed\": ${reparsed.get}")
  }

  // --- the compile fast path used by sample and Simpson ---

  "compile" should "lower the special functions to a closure" in
  {
    compile(parse("Gamma(x)"), x, env) match
      case Some(f) => assertClose(f(5.0), 24.0, 1e-9)
      case None    => fail("Gamma should compile")
    compile(parse("fact(x)"), x, env) match
      case Some(f) => assertClose(f(5.0), 120.0, 1e-9)
      case None    => fail("fact should compile")
  }

  it should "yield NaN out of domain, so sample drops the point" in
  {
    compile(parse("Gamma(x)"), x, env) match
      case Some(f) => assert(f(0.0).isNaN, "a pole must produce NaN for the sampler to drop")
      case None    => fail("Gamma should compile")
    // sample drops the non-finite points rather than reporting them
    val pts = sample(parse("Gamma(x)"), x, -3.0, 3.0, 13, env)
    assert(pts.forall((_, y) => !y.isNaN && !y.isInfinite), "sample must drop the poles")
    assert(pts.nonEmpty)
  }
