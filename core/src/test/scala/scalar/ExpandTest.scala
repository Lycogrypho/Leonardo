package it.grypho.scala.leonardo
package scalar

import core.*
import scalar.*
import scalar.Syntax.*
import org.scalatest.flatspec.AnyFlatSpec


class ExpandTest extends AnyFlatSpec:

  val x = _Variable("x")
  val y = _Variable("y")
  val z = _Variable("z")

  def envWith(bindings: (String, Double)*): Environment =
    bindings.foldLeft(new Environment())((e, kv) => e.withBinding(kv._1, _Number(kv._2)))

  def evalNum(expr: _Expression, bindings: (String, Double)*): Double =
    expr.eval(envWith(bindings*)) match
      case Right(_Number(v)) => v
      case other             => fail(s"expected a numeric result but got: $other")

  // --- distribution ---

  "expand(x * (y + z))" should "distribute to x*y + x*z" in
  {
    val result = Product(x, Sum(y, z)).expand()
    assert(result == Sum(Product(x, y), Product(x, z)))
  }

  "(expand((x + y) * z))" should "distribute to x*z + y*z" in
  {
    val result = Product(Sum(x, y), z).expand()
    assert(result == Sum(Product(x, z), Product(y, z)))
  }

  "expand((x+y) * (x+y)) at x=2, y=3" should "equal 25" in
  {
    val expr = Product(Sum(x, y), Sum(x, y))
    assert(math.abs(evalNum(expr.expand(), "x" -> 2.0, "y" -> 3.0) - 25.0) < 1e-4)
  }

  "expand((x+1) * (x+2)) at x=3" should "equal 20" in
  {
    val expr = Product(Sum(x, _Number(1)), Sum(x, _Number(2)))
    assert(math.abs(evalNum(expr.expand(), "x" -> 3.0) - 20.0) < 1e-4)
  }

  "expand(2 * (x + 3)) at x=5" should "equal 16" in
  {
    val expr = Product(_Number(2), Sum(x, _Number(3)))
    assert(math.abs(evalNum(expr.expand(), "x" -> 5.0) - 16.0) < 1e-4)
  }

  // --- power expansion ---

  "expand((x+y)^2) at x=2, y=3" should "equal 25" in
  {
    val expr = Power(Sum(x, y), _Number(2))
    assert(math.abs(evalNum(expr.expand(), "x" -> 2.0, "y" -> 3.0) - 25.0) < 1e-4)
  }

  "expand((x+1)^3) at x=2" should "equal 27" in
  {
    val expr = Power(Sum(x, _Number(1)), _Number(3))
    assert(math.abs(evalNum(expr.expand(), "x" -> 2.0) - 27.0) < 1e-4)
  }

  "expand((x+y)^4) at x=1, y=2" should "equal 81" in
  {
    val expr = Power(Sum(x, y), _Number(4))
    assert(math.abs(evalNum(expr.expand(), "x" -> 1.0, "y" -> 2.0) - 81.0) < 1e-4)
  }

  "expand((x+1)^1)" should "leave the sum unchanged" in
  {
    val result = Power(Sum(x, _Number(1)), _Number(1)).expand()
    assert(result == Sum(x, _Number(1)))
  }

  // non-sum base: expand should recurse but not distribute
  "expand(x^2) where base is not a sum" should "remain x^2" in
  {
    assert(Power(x, _Number(2)).expand() == Power(x, _Number(2)))
  }

  // --- non-integer or zero exponent: no expansion ---

  "expand((x+y)^0)" should "return (x+y)^0 without expanding" in
  {
    val result = Power(Sum(x, y), _Number(0)).expand()
    // expand does not evaluate — simplify() would fold this to 1
    assert(result == Power(Sum(x, y), _Number(0)))
  }

  "expand((x+y)^0.5)" should "return (x+y)^0.5 without expanding" in
  {
    val result = Power(Sum(x, y), _Number(0.5)).expand()
    assert(result == Power(Sum(x, y), _Number(0.5)))
  }

  // --- expand then simplify ---

  "expand((x+1)^2) then simplify at x=3" should "equal 16" in
  {
    val expr = Power(Sum(x, _Number(1)), _Number(2)).expand().simplify()
    assert(math.abs(evalNum(expr, "x" -> 3.0) - 16.0) < 1e-4)
  }

  "constant terms in (x+2)^2 simplify correctly" should "fold 2*2 to 4" in
  {
    // (x+2)^2 expands to x*x + x*2 + 2*x + 2*2
    // after simplify: x^2 + 2x + 2x + 4   (x*2 and 2*x not yet merged — single pass)
    // but numeric eval should still match (x+2)^2
    val expanded = Power(Sum(x, _Number(2)), _Number(2)).expand().simplify()
    assert(math.abs(evalNum(expanded, "x" -> 5.0) - 49.0) < 1e-4)
  }

  // --- nested structure ---

  "expand((x + y) * (x - y))" should "equal the numeric result of x^2 - y^2" in
  {
    // (x - y) = Sum(x, Product(-1, y))
    val xmy = Sum(x, Product(_Number(-1), y))
    val expr = Product(Sum(x, y), xmy).expand()
    // Numeric check: at x=5, y=3: (5+3)(5-3) = 16, 25-9 = 16
    assert(math.abs(evalNum(expr, "x" -> 5.0, "y" -> 3.0) - 16.0) < 1e-4)
  }

  // --- inverse trig: arguments must be expanded (regression for issue #20) ---

  "expand(asin(x * (y + z)))" should "distribute inside the argument" in
  {
    val result = Asin(Product(x, Sum(y, z))).expand()
    assert(result == Asin(Sum(Product(x, y), Product(x, z))))
  }

  "expand(acos(x * (y + z)))" should "distribute inside the argument" in
  {
    val result = Acos(Product(x, Sum(y, z))).expand()
    assert(result == Acos(Sum(Product(x, y), Product(x, z))))
  }

  "expand(atan(x * (y + z)))" should "distribute inside the argument" in
  {
    val result = Atan(Product(x, Sum(y, z))).expand()
    assert(result == Atan(Sum(Product(x, y), Product(x, z))))
  }

  "expand(asin((x+1)^2)) at x=0" should "equal asin(1)" in
  {
    val expr = Asin(Power(Sum(x, _Number(1)), _Number(2))).expand()
    assert(math.abs(evalNum(expr, "x" -> 0.0) - math.asin(1.0)) < 1e-5)
  }

  // --- _Heaviside: argument must be expanded (issue 3.3) ---

  "expand(step(x * (y + z)))" should "distribute inside the argument" in
  {
    val result = _Heaviside(Product(x, Sum(y, z))).expand()
    assert(result == _Heaviside(Sum(Product(x, y), Product(x, z))))
  }

  "expand(step((x+1)^2)) at x=-1" should "evaluate to step(0) = 1" in
  {
    val expr = _Heaviside(Power(Sum(x, _Number(1)), _Number(2))).expand()
    assert(math.abs(evalNum(expr, "x" -> -1.0) - 1.0) < 1e-5)
  }

  // --- F_0076: like terms fold while expanding, and the work is bounded ---

  /** The terms of a left-spine sum, counted without recursion. */
  private def termsOf(e: _Expression): Vector[_Expression] =
    Iterator.unfold(Option(e))(_.map {
      case Sum(l, r) => (r, Some(l))
      case last      => (last, None)
    }).toVector.reverse

  private def parse(s: String): _Expression = parser.Parser.parse(s).get

  "expand((a+b)^20)" should "be the 21 binomial terms, not 2^20 products" in
  {
    val (r, why) = expandReporting(parse("(a+b)^20"))
    assert(why.isEmpty)
    assert(termsOf(r).size == 21)
    // The coefficients ARE the binomial row: a = b = 1 sums them to 2^20, a = 1, b = -1 to 0.
    assert(math.abs(evalNum(r, "a" -> 1.0, "b" -> 1.0) - math.pow(2, 20)) < 1e-6)
    assert(math.abs(evalNum(r, "a" -> 1.0, "b" -> -1.0)) < 1e-6)
    assert(termsOf(r).contains(Product(_Number(184756), Product(Power(_Variable("a"), _Number(10)),
                                                                 Power(_Variable("b"), _Number(10))))))
  }

  it should "fold x + x and cancel what cancels" in
  {
    assert(Sum(x, x).expand() == Product(_Number(2), x))
    assert(parse("(x + 1)*(x - 1) - x^2").expand() == _Number(-1))
    assert(parse("(x+1)^20*(x-1)^20").expand() == parse("(x^2 - 1)^20").expand())
  }

  it should "keep the order terms and factors first appear in" in
  {
    // ((x^2 + 2x) + 1), the spelling the REPL's help text shows.
    assert(parse("(x + 1)^2").expand().toString == "(((x ^ 2.0) + (2.0 * x)) + 1.0)")
    assert(parse("y*(x + z)").expand() == Sum(Product(y, x), Product(y, z)))
  }

  it should "stay exact on exact input" in
  {
    val r = parser.Parser.parse("(x + 1/3)^2", exact = Some(30)).get.expand()
    assert(_Rational.containsExact(r))
    assert(!r.toString.contains("0.333"), s"an exact coefficient was demoted: $r")
    r.eval(new Environment().withBinding("x", _Rational(1))) match
      case Right(q: _Rational) => assert(q == _Rational(16).divide(_Rational(9)).getOrElse(fail("16/9")))
      case other               => fail(s"expected the exact 16/9, got $other")
  }

  it should "distribute a matrix leaf in order, without folding" in
  {
    // Matrix multiplication does not commute, so M*N + N*M must not become 2*M*N.
    val m = _MatrixValue(2, 2, Array(1.0, 2.0, 3.0, 4.0))
    val n = _MatrixValue(2, 2, Array(0.0, 1.0, 1.0, 0.0))
    val r = Product(Sum(m, n), Sum(m, n)).expand()
    def summands(e: _Expression): Vector[_Expression] = e match
      case Sum(a, b) => summands(a) ++ summands(b)
      case other     => Vector(other)
    assert(summands(r) == Vector(Product(m, m), Product(m, n), Product(n, m), Product(n, n)))
    assert(r.eval(new Environment()) == Product(Sum(m, n), Sum(m, n)).eval(new Environment()))
  }

  "an expansion past the size budget" should "be refused, left as written, with the reason" in
  {
    // (a+b+c+d)^20 has C(23, 3) = 1771 terms, past MaxExpandTerms.
    val (r, why) = expandReporting(parse("(a+b+c+d)^20"))
    assert(why.exists(_.contains(s"more than the limit of $MaxExpandTerms")), s"reason: $why")
    r match
      case Power(base, _Number(20.0)) => assert(termsOf(base).size == 4)
      case other                      => fail(s"expected the power left as written, got $other")
    // Everything under the budget still answers, with no reason attached.
    assert(expandReporting(parse("(a+b+c)^20"))._2.isEmpty)
  }

  "a product past the work budget" should "be refused before any term is built" in
  {
    val wide = (1 to 600).map(i => s"x$i").mkString("(", " + ", ")")
    val (_, why) = expandReporting(parse(s"$wide * $wide"))
    assert(why.exists(_.contains(s"600 by 600 terms exceeds the limit of $MaxExpandWork")), s"reason: $why")
  }
