package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

import core.*
import parser.Parser
import scalar.{Sum, Product, Ratio, Power}
import equation.{_Equation, _Comparison, CompareOp}

/** F_0224: the conventional display under `pretty on` -- minimal parentheses and binary minus.
 *
 *  **The property that keeps it honest is the round trip**: a reader may copy an answer back
 *  into the prompt, so the displayed text must re-parse to an expression of the same value.
 *  Structural `==` is not a valid check (`a - b` and `a + (-1)*b` are one tree, `(a + b) + c`
 *  and `a + (b + c)` two), so the comparison is numerical, at random points -- the
 *  `IntegralTableTest` rule.
 */
class ConventionalTest extends AnyFlatSpec:

  /** Full-precision leaves, so the comparison measures the notation and not display rounding. */
  private def leaf(e: _Expression): Option[String] = e match
    case r: _Rational => Some(r.exact)
    case n: _Number   => Some(_Number.render(n.d))
    case _            => None

  private def shown(e: _Expression): String = Conventional.render(e, leaf)

  private def parsed(src: String, exact: Option[Int] = None): _Expression =
    val p = Parser.parse(src, exact)
    assert(p.successful, s"'$src' must parse: $p")
    p.get

  private val x = _Variable("x")
  private val y = _Variable("y")

  "the conventional display" should "write the course review's three probes as a reader would" in {
    val two   = _Rational(2)
    val three = _Rational(3)
    val minus = _Rational(-1)
    // solveSystem's answer for x: (-1 * ((2 * y) + -3))
    assert(shown(_Equation(x, Product(minus, Sum(Product(two, y), _Rational(-3))))) == "x = -(2*y - 3)")
    assert(shown(parsed("4 - k*k", Some(30))) == "4 - k*k")
    // the definition F := [[2/3, -2/3]], echoed before the cells fold
    assert(shown(parsed("[[2/3, -2/3]]", Some(30))) == "[[2/3, -2/3]]")
    assert(shown(Ratio(two, three)) == "2/3")
  }

  it should "keep exactly the brackets the grammar needs" in {
    assert(shown(Power(Sum(x, y), _Number(2))) == "(x + y)^2.0")
    assert(shown(Power(_Number(-2), x)) == "(-2.0)^x")
    assert(shown(Power(Power(x, y), _Number(2))) == "(x^y)^2.0")
    assert(shown(Power(x, Power(y, _Number(2)))) == "x^y^2.0")
    assert(shown(Ratio(x, Product(y, _Number(2)))) == "x/(y*2.0)")
    assert(shown(Sum(x, Product(_Number(-1), Sum(y, _Number(1))))) == "x - (y + 1.0)")
    assert(shown(Sum(x, _Number(-3))) == "x - 3.0")
    assert(shown(Product(x, Product(_Number(-1), y))) == "x*(-y)")
    assert(shown(Product(_Number(2), _Equation(x, y))) == "2.0*(x = y)")
    assert(shown(_Comparison(Sum(x, y), CompareOp.Lt, _Number(1))) == "x + y < 1.0")
    assert(shown(Sum(Sum(x, y), Sum(_Number(1), y))) == "x + y + 1.0 + y")
    assert(shown(Product(Product(_Number(-1), x), y)) == "-x*y")
    // an exact leaf is not a bare token, so it is bracketed as a power base (the F_0046 rule)
    assert(shown(Power(Ratio(_Rational(2), _Rational(3)), x)) == "(2/3)^x")
  }

  it should "leave the arguments of a call to the call's own spelling" in {
    assert(shown(parsed("sin(x + -1*y) + derive(x^2, x)")) == "sin(x - y) + derive(x^2.0, x)")
  }

  it should "re-parse to an expression of the same value, on a generated corpus" in {
    val random = new scala.util.Random(224)
    def leafAt(): _Expression = random.nextInt(6) match
      case 0 => x
      case 1 => y
      case 2 => _Number((random.nextInt(9) - 4).toDouble)
      case 3 => _Number(random.nextInt(90) / 10.0 + 0.5)
      case 4 => _Rational(random.nextInt(7) - 3).divide(_Rational(random.nextInt(4) + 1)).getOrElse(_Rational(1))
      case _ => _Rational(-1)
    def tree(depth: Int): _Expression =
      if depth == 0 || random.nextInt(4) == 0 then leafAt()
      else random.nextInt(6) match
        case 0 => Sum(tree(depth - 1), tree(depth - 1))
        case 1 => Product(tree(depth - 1), tree(depth - 1))
        case 2 => Ratio(tree(depth - 1), tree(depth - 1))
        case 3 => Power(tree(depth - 1), _Number((random.nextInt(4) - 1).toDouble))
        case 4 => Product(_Number(-1), tree(depth - 1))
        case _ => Sum(tree(depth - 1), Product(_Rational(-1), tree(depth - 1)))
    def valueOf(e: _Expression, xv: Double, yv: Double): Option[Double] =
      e.eval(new Environment(5, Map("x" -> _Number(xv), "y" -> _Number(yv)))) match
        case Right(_Number(d)) if d.isFinite => Some(d)
        case _                               => None
    var compared = 0
    for _ <- 1 to 600 do
      val e    = tree(4)
      val text = shown(e)
      val back = parsed(text)
      for _ <- 1 to 3 do
        val (xv, yv) = (random.nextDouble() * 4 - 2, random.nextDouble() * 4 - 2)
        (valueOf(e, xv, yv), valueOf(back, xv, yv)) match
          case (Some(a), Some(b)) =>
            compared += 1
            assert(math.abs(a - b) <= 1e-9 * math.max(1.0, math.max(math.abs(a), math.abs(b))),
              s"$e shown as $text re-parses to $back: $a vs $b at x = $xv, y = $yv")
          case (a, b) => assert(a.isDefined == b.isDefined, s"$e shown as $text: $a vs $b")
    assert(compared > 1000, compared)
  }
