package it.grypho.scala.leonardo

import core.*
import scalar.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class IntegrationTest extends AnyFlatSpec:

  val x = _Variable("x")
  def env: Environment = new Environment()

  def evalDefInt(integrand: _Expression, v: _Variable, a: Double, b: Double): Double =
    _DefIntegral(integrand, v, _Number(a), _Number(b)).eval(env) match
      case Right(_Number(y)) => y
      case other             => fail(s"expected a numeric result but got: $other")

  "∫1 dx from 0 to 5" should "equal 5.0" in
  {
    assert(math.abs(evalDefInt(_Number(1), x, 0, 5) - 5.0) < 1e-4)
  }

  "∫x dx from 0 to 1" should "equal 0.5" in
  {
    assert(math.abs(evalDefInt(x, x, 0, 1) - 0.5) < 1e-4)
  }

  "∫x² dx from 0 to 1" should "equal 1/3" in
  {
    assert(math.abs(evalDefInt(Power(x, _Number(2)), x, 0, 1) - 1.0 / 3.0) < 1e-4)
  }

  "∫sin(x) dx from 0 to π" should "equal 2.0" in
  {
    assert(math.abs(evalDefInt(Sin(x), x, 0, math.Pi) - 2.0) < 1e-4)
  }

  "∫cos(x) dx from 0 to π/2" should "equal 1.0" in
  {
    assert(math.abs(evalDefInt(Cos(x), x, 0, math.Pi / 2) - 1.0) < 1e-4)
  }

  "∫exp(x) dx from 0 to 1" should "equal e - 1" in
  {
    assert(math.abs(evalDefInt(Exp(x), x, 0, 1) - (math.E - 1)) < 1e-4)
  }

  "∫x dx from 1 to 3" should "equal 4.0" in
  {
    assert(math.abs(evalDefInt(x, x, 1, 3) - 4.0) < 1e-4)
  }

  "∫(x + y) dx from 0 to 1 with y unbound" should "stay symbolic" in
  {
    val y = _Variable("y")
    _DefIntegral(Sum(x, y), x, _Number(0), _Number(1)).eval(env) match
      case Left(_)  => succeed
      case Right(v) => fail(s"expected symbolic but got $v")
  }

  "indefinite ∫x dx" should "stay symbolic" in
  {
    _Integral(x, x).eval(env) match
      case Left(_)  => succeed
      case Right(v) => fail(s"expected symbolic but got $v")
  }

  "parse+eval of \"integral(x, x, 0, 1)\"" should "equal 0.5" in
  {
    val result = Parser.parse("integral(x, x, 0, 1)")
    assert(result.successful, s"parse failed: $result")
    result.get.eval(env) match
      case Right(_Number(y)) => assert(math.abs(y - 0.5) < 1e-4)
      case other             => fail(s"expected a numeric result but got: $other")
  }

  // --- issue 2: step count scales with env.precision ---

  "∫x dx from 0 to 1 with precision=10" should "equal 0.5 with tight tolerance" in
  {
    val preciseEnv = new Environment(10)
    _DefIntegral(x, x, _Number(0), _Number(1)).eval(preciseEnv) match
      case Right(_Number(y)) => assert(math.abs(y - 0.5) < 1e-9)
      case other             => fail(s"expected numeric result but got: $other")
  }

  "∫x dx from 0 to 1 with precision=1" should "still approximate 0.5" in
  {
    val coarseEnv = new Environment(1)
    _DefIntegral(x, x, _Number(0), _Number(1)).eval(coarseEnv) match
      case Right(_Number(y)) => assert(math.abs(y - 0.5) < 1e-2)
      case other             => fail(s"expected numeric result but got: $other")
  }
