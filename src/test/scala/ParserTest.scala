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
    // the sign folds into the leading numeric coefficient (round-trip stability)
    ("-3k",                """(-3.0 * k)""",                  Product(_Number(-3), _Variable("k"))),
    ("sin(a)cos(b)",       """(sin(a) * cos(b))""",           Product(Sin(_Variable("a")), Cos(_Variable("b")))),
    ("pow(2, 10)",         """(2.0 ^ 10.0)""",               Power(_Number(2), _Number(10))),
    ("2 ^ 10",             """(2.0 ^ 10.0)""",               Power(_Number(2), _Number(10))),
    ("2 ^ 3 ^ 2",          """(2.0 ^ (3.0 ^ 2.0))""",        Power(_Number(2), Power(_Number(3), _Number(2)))),
    ("2 ^ 3 * 4",          """((2.0 ^ 3.0) * 4.0)""",        Product(Power(_Number(2), _Number(3)), _Number(4))),
    ("3x^2",               """(3.0 * (x ^ 2.0))""",          Product(_Number(3), Power(_Variable("x"), _Number(2)))),
    // unary minus in operand position (issue 26)
    ("3 * -x",             """(3.0 * (-1.0 * x))""",         Product(_Number(3), Product(_Number(-1), _Variable("x")))),
    ("3 + -x",             """(3.0 + (-1.0 * x))""",         Sum(_Number(3), Product(_Number(-1), _Variable("x")))),
    ("2^-x",               """(2.0 ^ (-1.0 * x))""",         Power(_Number(2), Product(_Number(-1), _Variable("x")))),
    ("2^-3",               """(2.0 ^ -3.0)""",               Power(_Number(2), _Number(-3))),
    ("3 - -2",             """(3.0 + (-1.0 * -2.0))""",      Sum(_Number(3), Product(_Number(-1), _Number(-2)))),
    // subtraction must never bind as implicit multiplication by a signed literal
    ("3-2",                """(3.0 + (-1.0 * 2.0))""",       Sum(_Number(3), Product(_Number(-1), _Number(2)))),
    ("x-2",                """(x + (-1.0 * 2.0))""",         Sum(_Variable("x"), Product(_Number(-1), _Number(2))))
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

  // --- reserved words are never variables ---

  "bare reserved words" should "fail to parse instead of becoming variables" in
  {
    for w <- List("sin", "cos", "exp", "log", "tan", "tg", "asin", "acos", "atan",
                  "pow", "transpose", "derive", "integral",
                  "simplify", "expand", "eval", "env", "vars",
                  "precision", "unset", "help", "quit", "exit") do
      assert(!Parser.parse(w).successful, s"'$w' must not parse as a variable")
  }

  "\"sin x\" (function without parentheses)" should "be a parse error, not sin·x" in
  {
    assert(!Parser.parse("sin x").successful)
  }

  "\"derive(x^2, sin)\" (reserved word as binder)" should "fail to parse" in
  {
    assert(!Parser.parse("derive(x^2, sin)").successful)
  }

  "names that merely start with a reserved word" should "still parse as variables" in
  {
    assert(parse("sina") == _Variable("sina"))
    assert(parse("evalx") == _Variable("evalx"))
    assert(parse("transposeM") == _Variable("transposeM"))
  }

  // --- issue 4.1: matrix literals and structural operator dispatch ---

  "\"[[1, 2], [3, 4]]\"" should "parse to a 2x2 _Matrix literal" in
  {
    assert(parse("[[1, 2], [3, 4]]")
      == matrix._Matrix(2, 2, Vector(_Number(1), _Number(2), _Number(3), _Number(4))))
  }

  "\"[[x, sin(x)]]\"" should "parse a row vector of arbitrary expressions" in
  {
    assert(parse("[[x, sin(x)]]")
      == matrix._Matrix(1, 2, Vector(_Variable("x"), Sin(_Variable("x")))))
  }

  "\"[[1, 2], [3]]\" (ragged rows)" should "fail to parse" in
  {
    assert(!Parser.parse("[[1, 2], [3]]").successful)
  }

  "\"[[1]] + [[2]]\"" should "dispatch to MatSum" in
  {
    assert(parse("[[1]] + [[2]]").isInstanceOf[matrix.MatSum])
  }

  "\"[[1]] - [[2]]\"" should "dispatch to MatSum of a MatScale(-1, ...)" in
  {
    parse("[[1]] - [[2]]") match
      case matrix.MatSum(_, matrix.MatScale(_Number(-1.0), _)) => succeed
      case other => fail(s"unexpected shape: $other")
  }

  "\"[[1]] * [[2]]\"" should "dispatch to MatProduct" in
  {
    assert(parse("[[1]] * [[2]]").isInstanceOf[matrix.MatProduct])
  }

  "\"2 * [[1]]\" and \"[[1]] * 2\"" should "both dispatch to MatScale" in
  {
    assert(parse("2 * [[1]]").isInstanceOf[matrix.MatScale])
    assert(parse("[[1]] * 2").isInstanceOf[matrix.MatScale])
  }

  "\"-[[1, 2]]\"" should "dispatch unary minus to MatScale(-1, ...)" in
  {
    assert(parse("-[[1, 2]]").isInstanceOf[matrix.MatScale])
  }

  "\"transpose([[1, 2], [3, 4]])\"" should "parse to a Transpose node" in
  {
    assert(parse("transpose([[1, 2], [3, 4]])").isInstanceOf[matrix.Transpose])
  }

  "matrix expressions" should "round-trip through toString with the same node types" in
  {
    for input <- List("[[1, 2], [3, 4]]", "[[1]] + [[2]]", "[[1]] * [[2]]",
                      "2 * [[1, 2]]", "transpose([[1, 2]])", "[[1]] - [[2]]") do
      val first = parse(input)
      val second = parse(first.toString)
      assert(second == first, s"round-trip changed \"$input\": $first vs $second")
  }

  // --- issue 26: subtraction vs implicit multiplication of a signed literal ---

  // Before the fix, the sign-carrying number literal let implicit multiplication
  // swallow the minus: "3-2" and "3 -2" silently evaluated to -6 (as 3 * (-2)).
  "\"3-2\", \"3 -2\" and \"3 - 2\"" should "all evaluate to 1.0, never to -6" in
  {
    for input <- List("3-2", "3 -2", "3 - 2") do
      parse(input).eval(new Environment()) match
        case Right(_Number(v)) => assert(v == 1.0, s"\"$input\" evaluated to $v")
        case other             => fail(s"\"$input\" did not reduce: $other")
  }

  "\"e-3\"" should "evaluate to e minus 3, not e times -3" in
  {
    parse("e-3").eval(new Environment()) match
      case Right(_Number(v)) => assert(math.abs(v - (math.E - 3.0)) < 1e-9)
      case other             => fail(s"did not reduce: $other")
  }

  "\"2e-3\"" should "stay a single scientific-notation literal" in
  {
    assert(parse("2e-3") == _Number(0.002))
  }

  "\"3(-2)\"" should "remain implicit multiplication (parenthesized operand)" in
  {
    assert(parse("3(-2)") == Product(_Number(3), _Number(-2)))
  }

  "\"integral(x, x, -1, 1)\"" should "accept signed limits" in
  {
    val e = parse("integral(x, x, -1, 1)")
    assert(e == _DefIntegral(_Variable("x"), _Variable("x"), _Number(-1), _Number(1)))
    e.eval(new Environment()) match
      case Right(_Number(v)) => assert(math.abs(v) < 1e-4)   // odd integrand, symmetric limits
      case other             => fail(s"did not reduce: $other")
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
