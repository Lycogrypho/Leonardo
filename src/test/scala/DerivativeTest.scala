package it.grypho.scala.leonardo

import it.grypho.scala.leonardo.expr._ //{Number, Variable, Power, Product}
//import it.grypho.scala.leonardo.linalg.DenseMatrix
import it.grypho.scala.leonardo.parser.Parser

import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec

/*
class DerivativeTest extends AnyFlatSpec with BeforeAndAfter
{
  val derVar = Variable("a")

  val matrixA = new DenseMatrix( List(
    List(new Variable("a"), new Number(1)),
    List(new Number(2), new Variable("b"))
  )
  )
  val matrixAderived = new DenseMatrix(List(
    List(new Number(1), new Number(0)),
    List(new Number(0), new Number(0))
  )
  )
  val exprB = derVar * Number(3)
  val exprC = Power(derVar, Number(2)) * Number(6)
  val exprD = Power(derVar, Number(3)) * Number(2)
  val exprE = Power(derVar, Number(2)) * Variable("b") * Number(5)
  val exprF = derVar * Variable("b") * Number(10)

  "derivative of a Number " should "be zero" in
    {
      assert (Number(3.0).derive(derVar) == Number(0))
    }

  "derivative of a Variaable in respect to another one " should "be zero" in
    {
      assert(Variable("d").derive(derVar) == Number(0))
    }

  "derivative of a first order polynomial " should "be a number" in
    {
      assert(exprB.derive(derVar).eval() == Number(3))
    }

  "derivative of a third order polynomial " should "be a second order polynomial" in
    {
      assert(exprD.derive(derVar).eval() == exprC.eval())
    }

  "derivative of polynomial (a^2 * b)" should "be (2 * a * b)" in
    {
      assert((Number(1) * derVar * derVar * Variable("b") ).derive(derVar).eval() == (derVar * Variable("b") * Number(2)).eval())
    }

  it should "be the same even if expressed through Powers" in
    {
      assert((Power(derVar, Number(2)) * Variable("b")).derive(derVar).eval() == (derVar * Variable("b") * Number(2)).eval() )
    }

  "derivative of polynomial (a^2 * b * c)" should "be (2 * a * b * c)" in
    {
      assert((Number(1) * Power(derVar, Number(2)) * Variable("b") * Variable("c")).derive(derVar).eval() == (derVar * Variable("b") * Number(2) * Variable("c")).eval())
    }

  it should "also when expressed via Product() instead of *" in
    {
      assert(Product(Number(1), Power(derVar, Number(2)), Variable("b"), Variable("c")).eval().derive(derVar).eval() == (derVar * Variable("b") * Number(2) * Variable("c")).eval() )
    }


  "derivative of polynomial (5 * a^2 * 3)" should "be (30 * a)" in
    {
      assert((Number(5) * Power(Variable("a"), Number(2)) * Number(3.0)).derive(derVar).eval() == (Number(30)*Variable("a")).eval())
    }


  "derivative of polynomial ((6 * a^2) * b)" should "be (12 * a * b)" in
    {
      assert((exprC * Variable("b")).derive(derVar).eval() == (Variable("a")*Variable("b")*Number(12)).eval())
    }

  "derivative of polynomial ((6 * b) * a^2)" should "be (12 * a * b)" in
    {
      assert(((Number(6) * Variable("b")) * Power(Variable("a"), Number(2))).derive(derVar).eval() == (Variable("a") * Variable("b") * Number(12)).eval())
    }


  "derivative of polynomial (5 * a^2 * b)" should "be (10 * a * b)" in
    {
      assert(exprE.derive(derVar).eval() == exprF.eval())
    }

  "Derivative of a Matrix" should "be the matrix of derivatives" in
    {
      assert(matrixA.derive(derVar).eval() == matrixAderived)

    }


}*/
