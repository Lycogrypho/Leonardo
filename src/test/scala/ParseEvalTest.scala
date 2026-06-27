package it.grypho.scala.leonardo

import core.*
import scalar.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class ParseEvalTest extends AnyFlatSpec:

  def env: Environment = new Environment()

  def parse(input: String): _Expression =
    val result = Parser.parse(input)
    assert(result.successful, s"parse failed for \"$input\": $result")
    result.get

  val numericCases: List[(String, Double)] = List(
    ("1",            1.0),
    ("1 + 2",        3.0),
    ("10 - 3",       7.0),
    ("3 * 4",        12.0),
    ("10 / 4",       2.5),
    ("2 + 3 * 4",    14.0),
    ("(2 + 3) * 4",  20.0),
    ("-2",           -2.0),
    ("exp(0)",       1.0),
    ("log(1)",       0.0),
    ("sin(0)",       0.0),
    ("cos(0)",       1.0),
    ("pow(2, 10)",   1024.0)
  )

  for (input, expected) <- numericCases do
    s"parse+eval of \"$input\"" should s"equal $expected" in
    {
      parse(input).eval(env) match
        case Right(_Number(x)) => assert(x == expected, s"got $x, expected $expected")
        case other             => fail(s"expected a numeric result but got: $other")
    }

  "parse+eval of \"3a\" with a = 2" should "equal 6.0" in
  {
    val e = env
    _Variable("a").set(_Number(2))(e)
    parse("3a").eval(e) match
      case Right(_Number(x)) => assert(x == 6.0)
      case other             => fail(s"expected 6.0 but got: $other")
  }

  "parse+eval of \"sin(a) + cos(a)\" with a unbound" should "remain symbolic" in
  {
    parse("sin(a) + cos(a)").eval(env) match
      case Left(_)   => succeed
      case Right(x)  => fail(s"expected symbolic result but got $x")
  }

  "parse+eval of \"sin(a) + cos(a)\" with a = 0" should "equal 1.0" in
  {
    val e = env
    _Variable("a").set(_Number(0))(e)
    parse("sin(a) + cos(a)").eval(e) match
      case Right(_Number(x)) => assert(x == 1.0)
      case other             => fail(s"expected 1.0 but got: $other")
  }

  "parse+eval of an expression with two references to the same variable" should "use the same binding" in
  {
    val e = env
    _Variable("x").set(_Number(5))(e)
    parse("x + x").eval(e) match
      case Right(_Number(x)) => assert(x == 10.0)
      case other             => fail(s"expected 10.0 but got: $other")
  }
