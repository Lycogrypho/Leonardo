package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import latex.ToLatex
import org.scalatest.flatspec.AnyFlatSpec


/** F_0056 — the order-based functions: `maximum`/`minimum` (the element-wise join, Decision A
 *  option 2), `max`/`min` (the reduction over all entries), `clamp`, `softplus`, their
 *  derivatives with the first argument favoured at a tie, and the kinks `differentiable` reports.
 */
class MinMaxTest extends AnyFlatSpec:

  private val env = new Environment()
  private val x   = _Variable("x")

  private def parse(s: String, exact: Option[Int] = None): _Expression =
    Parser.parse(s, exact) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def num(s: String, e: Environment = env): Double = parse(s).eval(e) match
    case Right(_Number(d)) => d
    case other             => fail(s"\"$s\" gave no number: $other")

  private def rows(s: String, e: Environment = env): Vector[Vector[Double]] = parse(s).eval(e) match
    case Right(m: _MatrixValue) => m.toVector.grouped(m.cols).toVector
    case other                  => fail(s"\"$s\" gave no dense matrix: $other")

  private def staysSymbolic(s: String, e: Environment = env): Unit =
    val node = parse(s)
    node.eval(e) match
      case Left(r) if r.getClass == node.getClass => ()
      case other                                  => fail(s"\"$s\" should stay symbolic, got $other")

  private def at(e: _Expression, d: Double): Double = e.eval(env.withBinding("x", _Number(d))) match
    case Right(_Number(v)) => v
    case other             => fail(s"$e at x = $d gave $other")

  // --- maximum / minimum: the join ---

  "maximum and minimum" should "pick among numbers, any number of them" in
  {
    assert(num("maximum(3, 5)") == 5 && num("minimum(3, 5)") == 3)
    assert(num("maximum(1, 7, 4)") == 7 && num("minimum(1, 7, -4)") == -4)
  }

  it should "return an exact operand as it is: a selection, not arithmetic" in
  {
    parse("maximum(1/2, 1/3)", Some(30)).eval(env) match
      case Right(r: _Rational) => assert(r == _Rational(1).divide(_Rational(2)).getOrElse(fail()))
      case other               => fail(s"expected the exact 1/2, got $other")
  }

  it should "work element-wise on matrices, a number spreading over every cell" in
  {
    assert(rows("maximum([[1, -2], [3, -4]], 0)") == Vector(Vector(1.0, 0), Vector(3.0, 0)))
    assert(rows("minimum([[1, 5]], [[4, 2]])") == Vector(Vector(1.0, 2)))
    staysSymbolic("maximum([[1, 2]], [[1], [2]])")   // shapes disagree
  }

  it should "keep a symbolic cell symbolic and fold the rest" in
  {
    parse("maximum([[x, 1]], 0)").eval(env) match
      case Left(m: _MatrixShaped) =>
        assert(m.children(1) == _Number(1))
        assert(m.children.head.isInstanceOf[_Join])
      case other => fail(s"expected a symbolic row, got $other")
  }

  it should "never guess the shape of a free name, and decline values with no order" in
  {
    staysSymbolic("maximum(x, 0)")
    assert(rows("maximum(x, 0)", env.withBinding("x", _MatrixValue(1, 2, Array(-1.0, 2.0)))) ==
      Vector(Vector(0.0, 2)))
    staysSymbolic("maximum(i, 1)")        // complex: no order
    staysSymbolic("maximum(true, false)") // truth values belong to the connectives
  }

  // --- max / min: the reduction ---

  "max and min" should "reduce over every entry, whatever the shape" in
  {
    assert(num("max([[3, 9], [1, 4]])") == 9 && num("min([[3, 9], [1, 4]])") == 1)
    assert(num("max([[2], [-7]])") == 2)
    assert(num("max(5)") == 5)
  }

  it should "stay exact on exact entries, and symbolic while an entry is" in
  {
    parse("max([[1/2, 1/3]])", Some(30)).eval(env) match
      case Right(_: _Rational) => succeed
      case other               => fail(s"expected an exact entry, got $other")
    staysSymbolic("max([[x, 1]])")
    staysSymbolic("max(x)")
  }

  // --- clamp ---

  "clamp" should "give the acceptance values" in
  {
    assert(num("clamp(150, 0, 100)") == 100 && num("clamp(-5, 0, 100)") == 0 && num("clamp(50, 0, 100)") == 50)
    staysSymbolic("clamp(x, 0, 100)")
    staysSymbolic("clamp(5, 10, 0)")      // lo > hi: no interval to clamp into
  }

  it should "saturate a column of inputs against scalar or column bounds" in
  {
    assert(rows("clamp([[150], [-5], [7]], 0, 100)") == Vector(Vector(100.0), Vector(0.0), Vector(7.0)))
    assert(rows("clamp([[3], [3]], [[0], [5]], [[2], [9]])") == Vector(Vector(2.0), Vector(5.0)))
  }

  // --- softplus ---

  "softplus" should "be the smooth max(0, x), and not overflow for a large k·x" in
  {
    assert(math.abs(num("softplus(0, 1)") - math.log(2)) < 1e-15)
    assert(math.abs(num("softplus(2, 1000)") - 2) < 1e-3)
    assert(math.abs(num("softplus(-2, 1000)")) < 1e-3)
    // ln(1 + e^10000) as written overflows to Infinity; the stable form is exact here.
    assert(num("softplus(10, 1000)") == 10.0)
    staysSymbolic("softplus(1, 0)")
    staysSymbolic("softplus(1, -2)")
  }

  // --- derivatives ---

  "the join's derivative" should "meet the acceptance: d/dx maximum(0, x)^2 = 2·maximum(0, x)" in
  {
    val d = simplify(derive(parse("maximum(0, x)^2"), x))
    for p <- List(-1.0, 0.0, 0.5, 2.0) do
      assert(math.abs(at(d, p) - 2 * math.max(0, p)) < 1e-12, s"at $p")
  }

  it should "favour the first argument at a tie, inside the subgradient" in
  {
    // maximum(x, 2x): slope 1 left of 0, 2 right of it; at 0 the first argument's slope, 1 — the
    // entry's original formula gave 1 + 2 = 3 there, outside [1, 2].
    val dmax = derive(parse("maximum(x, 2*x)"), x)
    assert(at(dmax, -1) == 1 && at(dmax, 1) == 2 && at(dmax, 0) == 1)
    val dmin = derive(parse("minimum(x, 2*x)"), x)
    assert(at(dmin, -1) == 2 && at(dmin, 1) == 1 && at(dmin, 0) == 1)
  }

  it should "carry through clamp and softplus" in
  {
    val dc = derive(parse("clamp(x, 0, 1)"), x)
    assert(at(dc, -1) == 0 && at(dc, 0.5) == 1 && at(dc, 2) == 0)
    assert(math.abs(at(derive(parse("softplus(x, 1)"), x), 0) - 0.5) < 1e-15)
  }

  // --- differentiability ---

  "differentiable" should "exclude exactly the kinks" in
  {
    def ok(s: String) = differentiableDomainOf(parse(s), x, DomainKind.Real, env).intervals
      .getOrElse(fail(s"no intervals for $s"))
    val m = ok("maximum(0, x)")
    assert(!m.exists(_.contains(0)) && m.exists(_.contains(-1)) && m.exists(_.contains(1)))
    val c = ok("clamp(x, 0, 1)")
    assert(!c.exists(_.contains(0)) && !c.exists(_.contains(1)) && c.exists(_.contains(0.5)))
  }

  it should "report a step differentiable everywhere but at its step (Decision B)" in
  {
    val d = differentiableDomainOf(_Heaviside(x), x, DomainKind.Real, env)
    val iv = d.intervals.getOrElse(fail("no intervals"))
    assert(!iv.exists(_.contains(0)) && iv.exists(_.contains(1)) && iv.exists(_.contains(-1)))
  }

  "softplus's domain" should "require k > 0" in
  {
    val d = domainOf(parse("softplus(x, k)"), x, DomainKind.Real, env)
    assert(d.constraints.contains(Constraint(_Variable("k"), Requirement.Positive)))
  }

  // --- fast path, notation, language ---

  "compile" should "cover the scalar functions, so sample and Simpson keep their fast path" in
  {
    for (s, p, want) <- List(("maximum(0, x, x/2)", 2.0, 2.0), ("minimum(0, x)", 2.0, 0.0),
                             ("clamp(x, 0, 1)", 3.0, 1.0), ("softplus(x, 1)", 0.0, math.log(2))) do
      compile(parse(s), x, env) match
        case Some(f) => assert(math.abs(f(p) - want) < 1e-15, s)
        case None    => fail(s"$s did not compile")
  }

  "LaTeX" should "use the \\max and \\min operators" in
  {
    assert(ToLatex(parse("maximum(a, b)")).contains("\\max"))
    assert(ToLatex(parse("min(A)")).contains("\\min"))
  }

  "the new words" should "round-trip and be reserved" in
  {
    for s <- List("maximum(a, b, c)", "minimum(a, 0)", "max(A)", "min(A)", "clamp(u, 0, 1)", "softplus(x, k)") do
      val e = parse(s)
      assert(parse(e.toString) == e, s"round-trip failed for $s: ${e.toString}")
    for w <- List("maximum", "minimum", "max", "min", "clamp", "softplus") do
      assert(Parser.ReservedWords.contains(w), w)
  }
