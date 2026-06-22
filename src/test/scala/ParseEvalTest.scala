package it.grypho.scala.leonardo

import it.grypho.scala.leonardo.parser.{Environment, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter

import expr._


class ParseEvalTest extends AnyFlatSpec with BeforeAndAfter:
  val parser = new Parser

  def env: Environment = new Environment()

  // (input string, expected numeric result)
  val numericCases = List(
    ("1",            1.0),
    ("1 + 2",        3.0),
    ("10 - 3",       7.0),
    ("3 * 4",        12.0),
    ("10 / 4",       2.5),
    ("2 + 3 * 4",    14.0),
    ("(2 + 3) * 4",  20.0),
    ("-2",           -2.0),
    ("3a",           6.0),   // a = 2 set below via env fixture
    ("exp(0)",       1.0),
    ("log(1)",       0.0),
    ("sin(0)",       0.0),
    ("cos(0)",       1.0),
    ("pow(2, 10)",   1024.0)
  )

  for (input, expected) <- numericCases if !input.contains("a") do
    s"parse+eval of \"$input\"" should s"equal $expected" in
    {
      val e   = env
      val ast = parser.parse(input).get
      ast.eval(e) match
        case Right(x) => assert(x == expected, s"got $x, expected $expected")
        case Left(sym) => fail(s"expected numeric result but got symbolic: $sym")
    }

  "parse+eval of \"3a\" with a = 2" should "equal 6.0" in
  {
    val e   = env
    val ast = parser.parse("3a").get
    _Variable("a").set(_Number(2))(e)
    ast.eval(e) match
      case Right(x)  => assert(x == 6.0)
      case Left(sym) => fail(s"expected 6.0 but got symbolic: $sym")
  }

  "parse+eval of \"sin(a) + cos(a)\" with a unbound" should "remain symbolic" in
  {
    val ast = parser.parse("sin(a) + cos(a)").get
    ast.eval(env) match
      case Left(_)  => succeed
      case Right(x) => fail(s"expected symbolic result but got $x")
  }

  "parse+eval of \"sin(a) + cos(a)\" with a = 0" should "equal 1.0" in
  {
    val e   = env
    val ast = parser.parse("sin(a) + cos(a)").get
    _Variable("a").set(_Number(0))(e)
    ast.eval(e) match
      case Right(x)  => assert(x == 1.0)
      case Left(sym) => fail(s"expected 1.0 but got symbolic: $sym")
  }

  "parse+eval of an expression with two references to the same variable" should "use the same binding" in
  {
    val e   = env
    val ast = parser.parse("x + x").get
    _Variable("x").set(_Number(5))(e)
    ast.eval(e) match
      case Right(x)  => assert(x == 10.0)
      case Left(sym) => fail(s"expected 10.0 but got symbolic: $sym")
  }
