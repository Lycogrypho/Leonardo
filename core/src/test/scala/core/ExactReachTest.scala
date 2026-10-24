package it.grypho.scala.leonardo
package core

import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0210: exact input keeps its exact answer through the nodes F_0171 did not reach.  Each case
 *  answered a `Double` before its fix (probed in exact mode, `.claudetools/p2-probe/out/`). */
class ExactReachTest extends AnyFlatSpec:

  private val env = new Environment()
  private def exact(s: String): _Expression = Parser.parse(s, Some(30)).get
  private def value(s: String): Either[_Expression, _Value] = exact(s).eval(env)
  private def q(n: Int, d: Int): _Rational = _Rational(n).divide(_Rational(d)).getOrElse(fail("no rational"))

  "limit" should "substitute an exact point exactly" in {
    assert(value("limit(x^2, x, 1/3)") == Right(q(1, 9)))
  }

  it should "apply L'Hôpital exactly at an exact 0/0" in {
    assert(value("limit((x^2 - 1/9)/(x - 1/3), x, 1/3)") == Right(q(2, 3)))
  }

  it should "answer a rational function at infinity in the input's tier" in {
    assert(value("limit((2*x^2 + x)/(3*x^2 + 1), x, inf)") == Right(q(2, 3)))
    assert(value("limit((x + 1)/(x^2 + 1), x, inf)") == Right(_Rational(0)))
  }

  "laurent" should "keep an exact coefficient exact, a constant beside the pole included" in {
    val r = value("laurent(1/(3*x^2), x, 0, 2, 1)").fold(identity, identity)
    assert(_Rational.containsExact(r) && !r.toString.contains("0.33333"), r)
  }

  "curl" should "hold an exact component exactly" in {
    val r = value("curl([[y/3], [0], [0]], x, y, z)").fold(identity, identity)
    assert(r.toString.contains("-1/3") && !r.toString.contains("0.33333"), r)
  }

  "normalize" should "write its exponents in the input's tier" in {
    val r = scalar.normalize(exact("x^2/3 + x/3"), _Variable("x"))
    assert(_Rational.containsExact(r) && !scalar.Sum(r, _Number(0)).toString.contains("^ 2.0"), r)
  }

  // --- the solvers: invented constants judged by the bindings too (the V3 rule) ---

  private val bound = env.withBinding("a", q(1, 3)).withBinding("b", q(1, 5))
  private def solved(s: String, in: Environment = bound): String =
    Parser.parse(s, Some(30)).get.eval(in).fold(identity, identity).toString

  "solve" should "keep letters bound to exact values exact" in {
    // Already exact before F_0210 (the sides are evaluated in the environment first); pinned.
    assert(solved("solve(a*x = b, x)") == "x = 3/5", solved("solve(a*x = b, x)"))
  }

  it should "write the symbolic quadratic's constants in the input's tier" in {
    val r = solved("solve(c*x^2 + x/3 = 0, x)", env)
    assert(!r.contains("0.66667") && !r.contains("2.0") && !r.contains("0.5"), r)
  }

  "an inequality" should "keep letters bound to exact values exact" in {
    assert(solved("solve(a*x - b > 0, x)") == "(x > 3/5)", solved("solve(a*x - b > 0, x)"))
  }

  "solveSystem" should "keep letters bound to exact values exact" in {
    val r = solved("solveSystem([[a*x + y = b], [x - y = 0]], x, y)")
    assert(r.contains("3/20") && !r.contains("0.15"), r)
  }

  "ode" should "answer an exact constant-coefficient problem exactly" in {
    // (y0 + b/a)·e^(aτ) − b/a; its `τ = target − t0` was built with a Double −1.
    assert(value("ode(y/3 + 1/5, y, t, 0, 1, 2)").exists(_.isInstanceOf[_Rational]),
           value("ode(y/3 + 1/5, y, t, 0, 1, 2)"))
  }

  it should "answer an exact variable-coefficient problem exactly" in {
    assert(value("ode(t/3, y, t, 0, 0, 1)") == Right(q(1, 6)), value("ode(t/3, y, t, 0, 0, 1)"))
  }

  // --- the transform tables: constants in tier, and a constant divisor read at all ---

  "laplace" should "read a constant divisor, in either mode" in {
    for parse <- List((s: String) => Parser.parse(s).get, exact) do
      assert(!parse("laplace(t/3, t, s)").eval(env).fold(identity, identity).toString.contains("laplace"))
      assert(!parse("laplace(exp(t/3), t, s)").eval(env).fold(identity, identity).toString.contains("laplace"))
  }

  it should "answer exact input exactly" in {
    val r = value("laplace(t^2/3 + exp(t/2), t, s)").fold(identity, identity)
    assert(!r.toString.contains("laplace") && _Rational.containsExact(r) && !r.toString.contains(".0"), r)
  }

  "ztrans" should "read a constant divisor and keep the geometric rule exact" in {
    assert(!Parser.parse("ztrans(n/3, n, z)").get.eval(env).fold(identity, identity).toString.contains("ztrans"))
    val r = value("ztrans((1/3)^n, n, z)").fold(identity, identity)
    assert(!r.toString.contains("0.33333") && !r.toString.contains("1.0"), r)
  }
