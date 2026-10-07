package it.grypho.scala.leonardo
package scalar

import core.*
import matrix.*
import parser.Parser
import vector.*
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

/** F_0058: `compile` over several variables, and `compileMatrix`.  The one-variable form is a
 *  wrapper over the same table, so its existing suites (Simpson, `sample`) are its regression. */
class CompileTest extends AnyFlatSpec:

  private val (x, y, z) = (_Variable("x"), _Variable("y"), _Variable("z"))
  private val xyz       = Seq(x, y, z)
  private val empty     = new Environment()

  private def parse(s: String): _Expression =
    val r = Parser.parse(s)
    if r.successful then r.get else fail(s"$s did not parse: $r")

  private def interpreted(e: _Expression, at: Array[Double], env: Environment = empty): Double =
    val bound = xyz.zip(at).foldLeft(env)((en, p) => en.withBinding(p._1.variable, _Number(p._2)))
    e.eval(bound) match
      case Right(_Number(d)) => d
      case other             => fail(s"$e did not evaluate at ${at.mkString(", ")}: $other")

  private def close(a: Double, b: Double): Boolean = math.abs(a - b) <= 1e-12 * math.max(1.0, math.abs(b))

  // Every point keeps x, y, z in (0.5, 2.5), so ln, sqrt and Gamma are all defined.
  private val sources = Seq(
    "x^3 + 2*x*y - z/4", "sin(x)*cos(y) + tan(z/3)", "exp(x - y) + ln(z) + log(x, 2)",
    "x^0.5 * y^z", "abs(x - y) + atan(z) + asin(x/3) + acos(y/3)",
    "maximum(x, y, z) - minimum(x, 2*y)", "clamp(x, y - 3, z) + softplus(x - y, 3)",
    "Gamma(x) + lgamma(y) + erf(z - 1) + Beta(x, y)", "gammaP(x, y) + betaI(x/3, y, z) + digamma(z)"
  )

  "compile over several variables" should "agree with eval within 1e-12 at random points" in {
    val rnd = new Random(58)
    for s <- sources do
      val e = parse(s)
      compile(e, xyz, empty) match
        case None => fail(s"$s did not compile")
        case Some(f) =>
          for _ <- 1 to 50 do
            val at = Array.fill(3)(0.5 + 2 * rnd.nextDouble())
            val (c, i) = (f(at), interpreted(e, at))
            assert(close(c, i), s"$s at ${at.mkString(", ")}: compiled $c, eval $i")
  }

  it should "answer NaN exactly where eval leaves the expression undefined" in {
    val e  = parse("clamp(x, y, z)")                 // lo > hi has no value
    val at = Array(1.0, 2.0, 0.5)
    assert(compile(e, xyz, empty).exists(f => f(at).isNaN))
    assert(e.eval(xyz.zip(at).foldLeft(empty)((en, p) => en.withBinding(p._1.variable, _Number(p._2)))).isLeft)
  }

  it should "read the arguments in the order the variables are given" in {
    val f = compile(parse("x - 10*y"), Seq(y, x), empty).getOrElse(fail("did not compile"))
    assert(f(Array(1.0, 2.0)) == -8.0)
  }

  it should "resolve other names from the environment, a listed variable shadowing a binding" in {
    val env = empty.withBinding("a", _Number(3.0)).withBinding("x", _Number(100.0))
    val f   = compile(parse("a*x + y"), Seq(x, y), env).getOrElse(fail("did not compile"))
    assert(f(Array(2.0, 1.0)) == 7.0)
  }

  it should "compile an exact constant as its value" in {
    val third = _Rational.of(1, 3).getOrElse(fail("no 1/3"))
    assert(compile(Product(third, x), Seq(x), empty).map(_(Array(3.0))).contains(1.0))
  }

  it should "decline what the table does not cover, rather than guess" in {
    assert(compile(parse("x + w"), Seq(x), empty).isEmpty, "a free name neither listed nor bound")
    assert(compile(parse("x + y"), Seq(x, x), empty).isEmpty, "a repeated variable")
    assert(compile(_Derivative(Sin(x), y), Seq(x, y), empty).isEmpty, "a functional")
    assert(compile(Sum(x, _Complex.of(0, 1)), Seq(x), empty).isEmpty, "a complex value")
    val m = empty.withBinding("w", _MatrixValue.identity(2))
    assert(compile(parse("x + w"), Seq(x), m).isEmpty, "a name bound to a matrix")
  }

  it should "give the one-variable form the same answers" in {
    val e = parse("sin(x)^2 + exp(-x)")
    val (one, many) = (compile(e, x, empty), compile(e, Seq(x), empty))
    for t <- Seq(-1.5, 0.0, 0.3, 2.0) do
      assert(one.map(_(t)) == many.map(_(Array(t))))
  }

  it should "be pure: interleaved calls on one closure answer as fresh ones do" in {
    val e = parse("x*y + sin(z)")
    val f = compile(e, xyz, empty).getOrElse(fail("did not compile"))
    val points = Vector.tabulate(200)(k => Array(k * 0.01, 1 - k * 0.003, k * 0.02))
    val fresh  = points.map(p => compile(e, xyz, empty).map(_(p)))
    val reused = Array(0.0, 0.0, 0.0)
    val shared = points.reverse.map { p => p.copyToArray(reused); f(reused) }.reverse
    assert(shared.map(Some(_)) == fresh)
  }

  "compileMatrix" should "evaluate a Jacobian's cells row-major, as eval does" in {
    val field = _Matrix.ofRows(Vector(parse("x^2*y")), Vector(parse("sin(y*z)")), Vector(parse("exp(x) - z")))
    val jac = _Jacobian(field, Vector(x, y, z)).eval(empty) match
      case Left(j: _Matrix) => j
      case other            => fail(s"no symbolic Jacobian: $other")
    val f  = compileMatrix(jac, xyz, empty).getOrElse(fail("did not compile"))
    val at = Array(1.2, 0.7, 2.1)
    val out = f(at)
    assert(out.length == 9)
    jac.children.zip(out).foreach((cell, c) => assert(close(c, interpreted(cell, at)), s"$cell"))
    assert(close(out(1), 1.2 * 1.2), "cell (1, 2) is d(x^2 y)/dy = x^2")
  }

  it should "return a fresh array per call" in {
    val f = compileMatrix(_Matrix.ofRows(Vector(x, y)), Seq(x, y), empty).getOrElse(fail("did not compile"))
    val first = f(Array(1.0, 2.0))
    f(Array(3.0, 4.0))
    assert(first.toSeq == Seq(1.0, 2.0))
  }

  it should "compile a dense matrix as its constant cells" in {
    val f = compileMatrix(_MatrixValue.identity(2), Seq(x), empty).getOrElse(fail("did not compile"))
    assert(f(Array(5.0)).toSeq == Seq(1.0, 0.0, 0.0, 1.0))
  }

  it should "decline a non-matrix, a matrix operation and a cell the table does not cover" in {
    assert(compileMatrix(x, Seq(x), empty).isEmpty)
    assert(compileMatrix(Transpose(_Matrix.ofRows(Vector(x))), Seq(x), empty).isEmpty)
    assert(compileMatrix(_Matrix.ofRows(Vector(x, _Derivative(x, x))), Seq(x), empty).isEmpty)
  }
