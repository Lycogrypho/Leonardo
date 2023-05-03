package it.grypho.scala.leonardo

import parser._ //{Environment, Parser}



object Main extends App
{
  println("Hello world!")
  def env = new Environment()

  def parser = new Parser(env)


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
    "sin(a)cos(b)"
  )

  val i = expressions.map(_.length).max

  for (expression <- expressions)
    {
      println(s"Parsing epression \"$expression\" ${" "*(i - expression.length)}\tas\t${parser.parse(expression).get}")
    }
}
