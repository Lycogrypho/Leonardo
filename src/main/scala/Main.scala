package it.grypho.scala.leonardo

import parser.Parser


@main def main(): Unit =
  val expressions = List(
    "1",
    "1 + 2",
    "(1 + 9.2)",
    "3*3E-5",
    "exp(1)",
    "sin(a)",
    "tg(x + 2)",
    "tg(x + log(2))",
    "1* exp(x)",
    "exp(cos(a))",
    "3a",
    "3sin(a)",
    "derive(cos(3x), x)",
    "-2",
    "+3sin(-a)",
    "-3k",
    "sin(a)cos(b)",
    "pow(2, 10)"
  )

  val width = expressions.map(_.length).max

  for expression <- expressions do
    val result = Parser.parse(expression)
    val output = if result.successful then result.get.toString
                 else s"[parse error: $result]"
    println(s"Parsing \"$expression\" ${"  " * (width - expression.length)}\tas\t$output")
