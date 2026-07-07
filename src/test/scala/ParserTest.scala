package it.grypho.scala.leonardo

import core.*
import scalar.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class ParserTest extends AnyFlatSpec:

  def parse(input: String): _Expression =
    val result = Parser.parse(input)
    assert(result.successful, s"parse failed for \"$input\": $result")
    result.get

  val expressionStrings: List[(String, String, _Expression)] = List(
    ("1",                  """1.0""",                         _Number(1)),
    ("1 + 2",              """(1.0 + 2.0)""",                 Sum(_Number(1), _Number(2))),
    ("(1 + 9.2)",          """(1.0 + 9.2)""",                 Sum(_Number(1), _Number(9.2))),
    ("3*3E-5",             """(3.0 * 3.0E-5)""",              Product(_Number(3), _Number(3.0E-5))),
    ("exp(1)",             """exp(1.0)""",                    Exp(_Number(1.0))),
    ("sin(a)",             """sin(a)""",                      Sin(_Variable("a"))),
    ("tg(x + 2)",          """tan((x + 2.0))""",               Tg(Sum(_Variable("x"), _Number(2.0)))),
    ("tg(x + log(2))",     """tan((x + log(2.0)))""",          Tg(Sum(_Variable("x"), Log(_Number(2.0))))),
    ("1* exp(x)",          """(1.0 * exp(x))""",              Product(_Number(1), Exp(_Variable("x")))),
    ("exp(cos(a))",        """exp(cos(a))""",                 Exp(Cos(_Variable("a")))),
    ("3a",                 """(3.0 * a)""",                   Product(_Number(3.0), _Variable("a"))),
    ("3sin(a)",            """(3.0 * sin(a))""",              Product(_Number(3.0), Sin(_Variable("a")))),
    ("derive(cos(3x), x)", """derive(cos((3.0 * x)), x)""",   _Derivative(Cos(Product(_Number(3.0), _Variable("x"))), _Variable("x"))),
    ("-2",                 """-2.0""",                        _Number(-2.0)),
    ("+3sin(-a)",          """(3.0 * sin((-1.0 * a)))""",     Product(_Number(3), Sin(Product(_Number(-1), _Variable("a"))))),
    ("-3k",                """(-1.0 * (3.0 * k))""",          Product(_Number(-1), Product(_Number(3.0), _Variable("k")))),
    ("sin(a)cos(b)",       """(sin(a) * cos(b))""",           Product(Sin(_Variable("a")), Cos(_Variable("b")))),
    ("pow(2, 10)",         """(2.0 ^ 10.0)""",               Power(_Number(2), _Number(10))),
    ("2 ^ 10",             """(2.0 ^ 10.0)""",               Power(_Number(2), _Number(10))),
    ("2 ^ 3 ^ 2",          """(2.0 ^ (3.0 ^ 2.0))""",        Power(_Number(2), Power(_Number(3), _Number(2)))),
    ("2 ^ 3 * 4",          """((2.0 ^ 3.0) * 4.0)""",        Product(Power(_Number(2), _Number(3)), _Number(4))),
    ("3x^2",               """(3.0 * (x ^ 2.0))""",          Product(_Number(3), Power(_Variable("x"), _Number(2))))
  )

  for s <- expressionStrings do
    s"Expression ${s._1} in the expression list " should s"be parsed as ${s._3}" in
    {
      assert(parse(s._1) == s._3)
    }

  for s <- expressionStrings do
    s"Internal representation of Expression ${s._1} " should s"be converted into string ${s._2}" in
    {
      assert(s._3.toString == s._2)
    }

  // --- unary minus on a literal is a toString fixpoint ---

  "\"-2\"" should "parse directly to _Number(-2.0), whose toString re-parses to itself" in
  {
    val first = parse("-2")
    assert(first == _Number(-2.0))
    assert(parse(first.toString) == first)
  }

  "\"-2.5\"" should "parse directly to _Number(-2.5)" in
  {
    assert(parse("-2.5") == _Number(-2.5))
  }

  "\"+2\"" should "parse to _Number(2.0), not a negated literal" in
  {
    assert(parse("+2") == _Number(2.0))
  }

  // --- issue 5: depth guard ---

  "600 levels of nested parentheses" should "fail with a parse error rather than StackOverflow" in
  {
    val deep = "(" * 600 + "1" + ")" * 600
    val result = Parser.parse(deep)
    assert(!result.successful)
  }

  "valid 10-level nesting" should "parse successfully" in
  {
    val nested = "(" * 10 + "1" + ")" * 10
    assert(parse(nested) == _Number(1.0))
  }

  // --- multi-character variable names (item 7) ---

  "theta" should "parse as _Variable(\"theta\")" in
  {
    assert(parse("theta") == _Variable("theta"))
  }

  "x1" should "parse as _Variable(\"x1\")" in
  {
    assert(parse("x1") == _Variable("x1"))
  }

  "sin(theta)" should "parse with multi-char argument" in
  {
    assert(parse("sin(theta)") == Sin(_Variable("theta")))
  }

  "derive(cos(theta), theta)" should "parse with multi-char differentiation variable" in
  {
    assert(parse("derive(cos(theta), theta)") ==
      _Derivative(Cos(_Variable("theta")), _Variable("theta")))
  }

  "3theta" should "parse as implicit multiplication" in
  {
    assert(parse("3theta") == Product(_Number(3.0), _Variable("theta")))
  }

  "alpha + beta" should "parse as Sum of two multi-char variables" in
  {
    assert(parse("alpha + beta") == Sum(_Variable("alpha"), _Variable("beta")))
  }

  // --- built-in constants (item 8) ---

  "pi" should "parse as _Number(math.Pi)" in
  {
    assert(parse("pi") == _Number(math.Pi))
  }

  "e" should "parse as _Number(math.E)" in
  {
    assert(parse("e") == _Number(math.E))
  }

  "sin(pi)" should "evaluate to zero" in
  {
    val env = new Environment()
    assert(math.abs(parse("sin(pi)").eval(env).toExpression.asInstanceOf[_Number].d) < 1e-10)
  }

  "pi * 2" should "parse as Product of pi-constant and 2" in
  {
    assert(parse("pi * 2") == Product(_Number(math.Pi), _Number(2)))
  }

  "exp(1)" should "still parse as Exp (not e followed by xp(1))" in
  {
    assert(parse("exp(1)") == Exp(_Number(1.0)))
  }

  "pine" should "parse as _Variable(\"pine\"), not pi * ne" in
  {
    assert(parse("pine") == _Variable("pine"))
  }

  "euler" should "parse as _Variable(\"euler\"), not e * uler" in
  {
    assert(parse("euler") == _Variable("euler"))
  }

  "e^2" should "parse as Power(_Number(math.E), _Number(2))" in
  {
    assert(parse("e^2") == Power(_Number(math.E), _Number(2)))
  }

  // --- tan as alias for tg (item 9) ---

  "tan(x)" should "parse as Tg(_Variable(\"x\"))" in
  {
    assert(parse("tan(x)") == Tg(_Variable("x")))
  }

  "tan(x + 2)" should "parse identically to tg(x + 2)" in
  {
    assert(parse("tan(x + 2)") == parse("tg(x + 2)"))
  }

  "tan(0)" should "have toString \"tan(0.0)\"" in
  {
    assert(parse("tan(0)").toString == "tan(0.0)")
  }

  "tg(x)" should "still parse (backward compat) and equal Tg(_Variable(\"x\"))" in
  {
    assert(parse("tg(x)") == Tg(_Variable("x")))
  }

  "Tg.toString" should "emit tan(...) not tg(...)" in
  {
    assert(Tg(_Variable("x")).toString == "tan(x)")
  }
