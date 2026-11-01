package it.grypho.scala.leonardo
package core

import scalar.{Product, Ratio, Sum}
import org.scalatest.flatspec.AnyFlatSpec


/** F_0248 -- exact `+ − × ÷` are held to two budgets before the `BigInt` work is done: the size of
 *  the result, and the gcd reduction of a fraction's.  Without them a share link replaying
 *  `a := 2^500000`, `b := a*a`, `c := b*b`, ... doubled the operand per line and froze the tab. */
class ExactOperandBudgetTest extends AnyFlatSpec:

  private val env = new Environment()
  private def twoTo(k: Int): _Rational = _Rational(BigInt(2).pow(k))
  private def frac(numBits: Int, denBits: Int): _Rational =
    _Rational.of(BigInt(2).pow(numBits) + 1, BigInt(3).pow((denBits * 0.631).toInt) + 2).getOrElse(fail("no fraction"))

  private def stays(e: _Expression, at: Environment): Boolean = e.eval(at) == Left(e)

  "an exact operation" should "stay unevaluated when its result would pass the size budget" in {
    val small = env.withLimit(Limit.MaxExactOperandBits, 1000)
    val (x, y) = (twoTo(600), twoTo(600))   // a product: 601 + 601 bits > 1000
    val big    = twoTo(1200)
    for e <- Seq(Product(x, y), Ratio(big, _Rational(3)), Sum(big, _Rational(1)),
                 Sum(big, Product(_Rational(-1), _Rational(5)))) do
      assert(stays(e, small), e)
    assert(Product(twoTo(400), twoTo(400)).eval(small) == Right(twoTo(800)))
  }

  it should "size a sum of integers as the larger operand, not as a product (F_0256)" in {
    val small = env.withLimit(Limit.MaxExactOperandBits, 1000)
    // 601 bits each: the sum has 602.  It was refused as though it had 1202.
    assert(Sum(twoTo(600), twoTo(600)).eval(small) == Right(twoTo(601)))
  }

  it should "hold the gcd a quotient of two integers runs (F_0256)" in {
    val small = env.withLimit(Limit.MaxExactReductionWork, 1000000)
    // a/b reduces by gcd(a, b): about 2002 x 2062 bits > 1e6, though each denominator is one bit.
    val (a, b) = (_Rational(BigInt(2).pow(2000) + 1), _Rational(BigInt(3).pow(1300)))
    assert(stays(Ratio(a, b), small))
    // A large integer over a small one costs nothing to reduce and is answered.
    assert(Ratio(twoTo(2000), _Rational(2)).eval(small) == Right(twoTo(1999)))
    // The audit's probe at the defaults: 23 s on the JVM before, now refused before any work.
    val (p, q) = (_Rational(BigInt(3).pow(330000)), _Rational(BigInt(5).pow(225000)))
    val start  = System.nanoTime
    assert(stays(Ratio(p, q), env))
    assert((System.nanoTime - start) / 1e9 < 1.0)
  }

  "a power of a fraction" should "reduce the base once and run no gcd on the power (F_0256)" in {
    val base  = _Rational.of(BigInt(3), BigInt(7)).getOrElse(fail("no fraction"))
    val start = System.nanoTime
    val p     = base.pow(300000).getOrElse(fail("declined"))   // 17.9 s on the JVM before
    // 8 s, not 2: Node took 2.8 s in a loaded full run (the BigInt powers alone), and the gcd this
    // pins as gone costs several times that on either platform.
    assert((System.nanoTime - start) / 1e9 < 8.0)
    assert(p.num == BigInt(3).pow(300000) && p.den == BigInt(7).pow(300000))
    // An unreduced base still comes out in lowest terms, the sign in the numerator.
    val unreduced = _Rational.of(BigInt(-4), BigInt(6)).getOrElse(fail("no fraction"))
    assert(unreduced.pow(-3).contains(_Rational.of(BigInt(-27), BigInt(8)).getOrElse(fail("no fraction"))))
    assert(unreduced.pow(3, GcdPolicy.Lazy, _Rational.MaxExactPowerBits)
      .contains(_Rational.of(BigInt(-8), BigInt(27)).getOrElse(fail("no fraction"))))
  }

  it should "hold a fraction's reduction work, while an integer is held only by its size" in {
    // 2000-bit fractions: (4000 result bits) x (about 4000 denominator bits) = 1.6e7.
    val small = env.withLimit(Limit.MaxExactReductionWork, 1000000)
    val (x, y) = (frac(2000, 2000), frac(2000, 2000))
    assert(stays(Product(x, y), small) && stays(Sum(x, y), small))
    // Two 3000-bit integers: 6000 result bits, a denominator of one bit -- well inside.
    assert(Product(twoTo(3000), twoTo(3000)).eval(small) == Right(twoTo(6000)))
  }

  it should "hold a surd operand to the same budgets" in {
    val small = env.withLimit(Limit.MaxExactOperandBits, 1000)
    val root  = _Surd.sqrtOf(_Rational(2)).getOrElse(fail("no root"))
    assert(stays(Product(root, twoTo(1200)), small))
    assert(Product(root, twoTo(10)).eval(small).isRight)
  }

  "the defaults" should "admit what a session legitimately builds" in {
    // A 100000-bit integer times a small one, as `fact(10000) * 2` (118k bits) needs.
    assert(Product(twoTo(100000), _Rational(3)).eval(env).isRight)
    assert(Limit.MaxExactOperandBits.default == (1L << 20) && Limit.MaxExactReductionWork.default == (1L << 32))
  }

  private def exactly(s: String, at: Environment = env): Either[_Expression, _Value] =
    parser.Parser.parse(s, Some(30)).get.eval(at)

  "a kernel over a field" should "be held to the run's total reduction work (F_0264)" in {
    // 600 values with distinct denominators: each step is within the per-operation budget, the
    // run is not -- 18 s on the JVM before.  It now stays unevaluated, promptly.
    val start = System.nanoTime
    assert(exactly("variance(tabulate(1/(k^3 + 7), k, 1, 600))").isLeft)
    assert((System.nanoTime - start) / 1e9 < 5.0)
    // A small exact sample keeps its exact answer.
    assert(exactly("variance([[1/2, 1/3, 1/6]])") == Right(_Rational.of(BigInt(1), BigInt(36)).getOrElse(fail("no fraction"))))
  }

  it should "hold det, inv and lu to the same budget" in {
    val tight = env.withLimit(Limit.MaxExactReductionWork, 50)
    for f <- Seq("det", "inv", "lu") do
      val call = s"$f([[1/3, 1/5], [1/7, 1/11]])"
      assert(exactly(call).isRight || exactly(call).left.exists(!_.toString.startsWith(s"$f(")), call)
      assert(exactly(call, tight).left.exists(_.toString.startsWith(s"$f(")), s"$call under a tight budget")
  }
