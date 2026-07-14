package it.grypho.scala.leonardo

import core.*
import scalar.*
import scalar.Syntax.*
import org.scalatest.flatspec.AnyFlatSpec


class AnalysisTest extends AnyFlatSpec:

  val x = _Variable("x")
  val y = _Variable("y")
  val n = _Number(3.0)

  // --- atomic cases ---

  "_Number" should "not depend on any variable" in
  {
    assert(!n.dependsOn(x))
  }

  "_Variable with matching name" should "depend on that variable" in
  {
    assert(x.dependsOn(x))
  }

  "_Variable with different name" should "not depend on other variable" in
  {
    assert(!x.dependsOn(y))
  }

  // --- binary operations ---

  "Sum(x, n)" should "depend on x" in
  {
    assert(Sum(x, n).dependsOn(x))
  }

  "Sum(n, n)" should "not depend on x" in
  {
    assert(!Sum(n, n).dependsOn(x))
  }

  "Product(x, n)" should "depend on x" in
  {
    assert(Product(x, n).dependsOn(x))
  }

  "Product(n, n)" should "not depend on x" in
  {
    assert(!Product(n, n).dependsOn(x))
  }

  "Ratio(x, n)" should "depend on x" in
  {
    assert(Ratio(x, n).dependsOn(x))
  }

  "Ratio(n, x)" should "depend on x" in
  {
    assert(Ratio(n, x).dependsOn(x))
  }

  "Ratio(n, n)" should "not depend on x" in
  {
    assert(!Ratio(n, n).dependsOn(x))
  }

  "Power(x, n)" should "depend on x" in
  {
    assert(Power(x, n).dependsOn(x))
  }

  "Power(n, x)" should "depend on x" in
  {
    assert(Power(n, x).dependsOn(x))
  }

  // --- unary functions ---

  "Exp(x)" should "depend on x" in
  {
    assert(Exp(x).dependsOn(x))
  }

  "Exp(n)" should "not depend on x" in
  {
    assert(!Exp(n).dependsOn(x))
  }

  "Ln(x)" should "depend on x" in
  {
    assert(Ln(x).dependsOn(x))
  }

  "Sin(x)" should "depend on x" in
  {
    assert(Sin(x).dependsOn(x))
  }

  "Cos(x)" should "depend on x" in
  {
    assert(Cos(x).dependsOn(x))
  }

  "Tg(x)" should "depend on x" in
  {
    assert(Tg(x).dependsOn(x))
  }

  "Asin(x)" should "depend on x" in
  {
    assert(Asin(x).dependsOn(x))
  }

  "Acos(x)" should "depend on x" in
  {
    assert(Acos(x).dependsOn(x))
  }

  "Atan(x)" should "depend on x" in
  {
    assert(Atan(x).dependsOn(x))
  }

  "Asin(y)" should "not depend on x" in
  {
    assert(!Asin(_Variable("y")).dependsOn(x))
  }

  // --- functional nodes ---

  "_Derivative(x^2, x)" should "depend on x" in
  {
    assert(_Derivative(Power(x, _Number(2)), x).dependsOn(x))
  }

  "_Derivative(n, x)" should "not depend on x (constant integrand)" in
  {
    assert(!_Derivative(n, x).dependsOn(x))
  }

  "_Integral(x, x)" should "depend on x" in
  {
    assert(_Integral(x, x).dependsOn(x))
  }

  "_DefIntegral(x^2, x, 0, 1)" should "depend on x via integrand" in
  {
    assert(_DefIntegral(Power(x, _Number(2)), x, _Number(0), _Number(1)).dependsOn(x))
  }

  "_DefIntegral(n, x, x, 1)" should "depend on x via lower bound" in
  {
    assert(_DefIntegral(n, x, x, _Number(1)).dependsOn(x))
  }

  "_DefIntegral(n, x, 0, x)" should "depend on x via upper bound" in
  {
    assert(_DefIntegral(n, x, _Number(0), x).dependsOn(x))
  }

  "_DefIntegral(n, x, 0, 1)" should "not depend on x" in
  {
    assert(!_DefIntegral(n, x, _Number(0), _Number(1)).dependsOn(x))
  }

  // --- catch-all: unknown _Expression subtype must return false, not throw ---

  "dependsOn with an unknown _Expression subtype" should "return false without MatchError" in
  {
    val unknown = new _Expression:
      def eval(env: Environment): Either[_Expression, _Value] = Left(this)
      def children: List[_Expression] = List.empty
      def rebuild(c: List[_Expression]): _Expression = this
    assert(!unknown.dependsOn(x))
  }
