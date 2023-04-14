package it.grypho.scala.leonardo

import parser._ //{Environment, Parser}



object Main extends App
{
  println("Hello world!")
  def env = new Environment()

  def parser = new Parser(env)


  println(parser.parse("1"))
  println(parser.parse("1 + 2"))
  println(parser.parse("(1 + 2)"))
  println(parser.parse("3*3"))
  println(parser.parse("exp(1)"))
  println(parser.parse("sin(a)"))
  println(parser.parse("tg(x + 2)"))
  println(parser.parse("tg(x + log(2))"))
  println(parser.parse("1* exp(x)"))
  println(parser.parse("exp(cos(a))"))
  println(parser.parse("3a"))
  println(parser.parse("3sin(a)"))

}
