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
    val (x, y) = (twoTo(600), twoTo(600))   // 601 + 601 bits > 1000
    for e <- Seq(Sum(x, y), Product(x, y), Ratio(x, y), Sum(x, Product(_Rational(-1), y))) do
      assert(stays(e, small), e)
    assert(Product(twoTo(400), twoTo(400)).eval(small) == Right(twoTo(800)))
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
