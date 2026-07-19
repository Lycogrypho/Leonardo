package it.grypho.scala.leonardo
package equation

import core.*
import scalar.*
import equation.*
import matrix.*
import parser.Parser
import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


class SolveSystemTest extends AnyFlatSpec:

  val x = _Variable("x")
  val y = _Variable("y")
  val z = _Variable("z")

  def parse(input: String): _Expression =
    val r = Parser.parse(input)
    assert(r.successful, s"parse failed for \"$input\": $r")
    r.get

  def solEq(sol: List[_Equation], v: _Variable): Double =
    sol.find(_.lhs == v).map(_.rhs match
      case _Number(d) => d
      case other      => fail(s"expected numeric RHS but got: $other")
    ).getOrElse(fail(s"no solution found for $v"))

  // --- dense 2Ã—2 system ---

  "2x + 3y = 7 / x - y = 1" should "give x=2, y=1" in
  {
    val eq1 = _Equation(Sum(Product(_Number(2), x), Product(_Number(3), y)), _Number(7))
    val eq2 = _Equation(Sum(x, Product(_Number(-1), y)), _Number(1))
    solveSystem(List(eq1, eq2), List(x, y)) match
      case None    => fail("expected a solution")
      case Some(s) =>
        assert(math.abs(solEq(s, x) - 2.0) < 1e-9)
        assert(math.abs(solEq(s, y) - 1.0) < 1e-9)
  }

  "2x + y = 5 / x + 3y = 10" should "give x=1, y=3" in
  {
    val eq1 = _Equation(Sum(Product(_Number(2), x), y), _Number(5))
    val eq2 = _Equation(Sum(x, Product(_Number(3), y)), _Number(10))
    solveSystem(List(eq1, eq2), List(x, y)) match
      case None    => fail("expected a solution")
      case Some(s) =>
        assert(math.abs(solEq(s, x) - 1.0) < 1e-9)
        assert(math.abs(solEq(s, y) - 3.0) < 1e-9)
  }

  // --- dense 3Ã—3 system: x+y+z=6, x-y-z=-2, 2x+y-z=2 â†’ x=2,y=1,z=3 ---

  "3Ã—3 integer system" should "give x=2, y=1, z=3" in
  {
    def eq(l: _Expression, r: Double) = _Equation(l, _Number(r))
    val e1 = eq(Sum(Sum(x, y), z), 6)
    val e2 = eq(Sum(Sum(x, Product(_Number(-1), y)), Product(_Number(-1), z)), -2)
    val e3 = eq(Sum(Sum(Product(_Number(2), x), y), Product(_Number(-1), z)), 2)
    solveSystem(List(e1, e2, e3), List(x, y, z)) match
      case None    => fail("expected a solution")
      case Some(s) =>
        assert(math.abs(solEq(s, x) - 2.0) < 1e-9)
        assert(math.abs(solEq(s, y) - 1.0) < 1e-9)
        assert(math.abs(solEq(s, z) - 3.0) < 1e-9)
  }

  // --- singular system ---

  "x + y = 3 / x + y = 5 (inconsistent)" should "give None" in
  {
    val eq1 = _Equation(Sum(x, y), _Number(3))
    val eq2 = _Equation(Sum(x, y), _Number(5))
    assert(solveSystem(List(eq1, eq2), List(x, y)).isEmpty)
  }

  // --- size mismatch ---

  "more equations than variables" should "give None" in
  {
    val eq1 = _Equation(x, _Number(1))
    val eq2 = _Equation(x, _Number(2))
    assert(solveSystem(List(eq1, eq2), List(x)).isEmpty)
  }

  "empty system" should "give None" in
  {
    assert(solveSystem(List.empty, List.empty).isEmpty)
  }

  // --- nonlinear detection ---

  "x^2 = 4 / y = 1 (nonlinear in x)" should "give None" in
  {
    val eq1 = _Equation(Power(x, _Number(2)), _Number(4))
    val eq2 = _Equation(y, _Number(1))
    assert(solveSystem(List(eq1, eq2), List(x, y)).isEmpty)
  }

  // --- symbolic coefficients (symbolic path) ---

  "a*x + y = 5 / x - y = 1 with a symbolic" should "give symbolic solution" in
  {
    val a   = _Variable("a")
    val eq1 = _Equation(Sum(Product(a, x), y), _Number(5))
    val eq2 = _Equation(Sum(x, Product(_Number(-1), y)), _Number(1))
    solveSystem(List(eq1, eq2), List(x, y)) match
      case None    => fail("expected a solution")
      case Some(s) => assert(s.forall(_.lhs.isInstanceOf[_Variable]))
  }

  "symbolic coefficients folded through env" should "give numeric answer when a is bound" in
  {
    val a   = _Variable("a")
    val env = new Environment().withBinding("a", _Number(2))
    val eq1 = _Equation(Sum(Product(a, x), y), _Number(5))
    val eq2 = _Equation(Sum(x, Product(_Number(-1), y)), _Number(1))
    // With a=2: 2x + y = 5, x - y = 1 â†’ x=2, y=1
    solveSystem(List(eq1, eq2), List(x, y), env) match
      case None    => fail("expected a solution")
      case Some(s) =>
        assert(math.abs(solEq(s, x) - 2.0) < 1e-9)
        assert(math.abs(solEq(s, y) - 1.0) < 1e-9)
  }

  // --- parser round-trip ---

  "\"solveSystem([[2*x + y = 5, x + 3*y = 10]], x, y)\"" should "parse to _SolveSystem and round-trip" in
  {
    val first = parse("solveSystem([[2*x + y = 5, x + 3*y = 10]], x, y)")
    assert(first.isInstanceOf[_SolveSystem])
    assert(parse(first.toString) == first)
  }

  "bare \"solveSystem\"" should "be a reserved word" in
  {
    assert(!Parser.parse("solveSystem").successful)
  }

  // --- _SolveSystem.eval ---

  "_SolveSystem.eval with a 2Ã—2 system" should "return a row-vector of solutions" in
  {
    val node = parse("solveSystem([[2*x + y = 5, x + 3*y = 10]], x, y)")
    node.eval(new Environment()) match
      case Left(m: _Matrix) =>
        assert(m.rows == 1 && m.cols == 2)
        m.elems.foreach { case _Equation(_, _Number(_)) => succeed; case other => fail(s"unexpected: $other") }
      case other => fail(s"unexpected: $other")
  }

  "_SolveSystem.eval with a 1-variable system" should "return a single equation" in
  {
    val node = parse("solveSystem([[2*x = 6]], x)")
    node.eval(new Environment()) match
      case Left(_Equation(`x`, _Number(3.0))) => succeed
      case other => fail(s"unexpected: $other")
  }

  "_SolveSystem with a singular system" should "stay symbolic" in
  {
    val node = parse("solveSystem([[x + y = 3, x + y = 5]], x, y)")
    assert(node.eval(new Environment()) == Left(node))
  }

  // --- REPL flow ---

  "solveSystem in the REPL" should "display solutions as a row-vector" in
  {
    val s = Session()
    assert(s.execute("solveSystem([[2*x + y = 5, x + 3*y = 10]], x, y)") == "[[x = 1.0, y = 3.0]]")
  }

  "solveSystem with a named equation matrix" should "work after binding S := [[...]]" in
  {
    val s = Session()
    s.execute("S := [[2*x + y = 5, x + 3*y = 10]]")
    assert(s.execute("solveSystem(S, x, y)") == "[[x = 1.0, y = 3.0]]")
  }

  "solveSystem with a bound coefficient" should "fold it numerically" in
  {
    val s = Session()
    s.execute("a := 2")
    // a=2: a*x + y = 5, x - y = 1 â†’ 2x + y = 5, x - y = 1 â†’ x=2, y=1
    assert(s.execute("solveSystem([[a*x + y = 5, x - y = 1]], x, y)") == "[[x = 2.0, y = 1.0]]")
  }
