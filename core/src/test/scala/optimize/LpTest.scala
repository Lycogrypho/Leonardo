package it.grypho.scala.leonardo
package optimize

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0011: exact linear programming, the simplex method over the rationals with Bland's rule.
 *  Every optimum is a textbook value, worked by hand. */
class LpTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String, exact: Option[Int] = None): _Expression =
    Parser.parse(s, exact) match
      case Parser.Success(e, _) => e
      case other                => fail(s"$s did not parse: $other")

  private def solved(s: String, exact: Option[Int] = None): LpResult = parse(s, exact) match
    case n: _LP => n.detailed(env).fold(why => fail(s"$s declined: $why"), identity)
    case other  => fail(s"$s parsed as $other")

  private def failure(s: String): LpFailure = parse(s) match
    case n: _LP => n.detailed(env).fold(identity, r => fail(s"$s solved: $r"))
    case other  => fail(s"$s parsed as $other")

  private def doubles(v: Vector[_Value]): Vector[Double] = v.map {
    case _Number(d) => d
    case other      => fail(s"not a number: $other")
  }

  "lp" should "solve the Wyndor problem: max 3x + 5y on x <= 4, 2y <= 12, 3x + 2y <= 18 at (2, 6)" in {
    val r = solved("lp([[-3], [-5]], [[1, 0], [0, 2], [3, 2]], [[4], [12], [18]], 0, inf)")
    assert(doubles(r.x) == Vector(2.0, 6.0) && r.objective == _Number(-36.0), r)
    assert(r.active.toSet == Set(QpConstraint.Row(1), QpConstraint.Row(2)), r.active)
  }

  it should "honour an equality and the bounds" in {
    // min x + y on x + 2y = 4, x, y >= 0: (0, 2), value 2.
    val r = solved("lp([[1], [1]], 0, 0, [[1, 2]], [[4]], 0, inf)")
    assert(doubles(r.x) == Vector(0.0, 2.0) && r.objective == _Number(2.0))
    // A free variable and a finite lower bound: min x on -x <= 5 is x = -5; on x >= -3 it is -3.
    assert(doubles(solved("lp(1, [[-1]], [[5]], -inf, inf)").x) == Vector(-5.0))
    assert(doubles(solved("lp(1, 0, 0, -3, inf)").x) == Vector(-3.0))
    // An upper bound: max x on x <= 7 (bound) is 7.
    assert(doubles(solved("lp(-1, 0, 0, 0, 7)").x) == Vector(7.0))
  }

  it should "be exact in exact mode: min -x - y on 3x + y <= 1, x + 3y <= 1 is -1/2 at (1/4, 1/4)" in {
    val r = solved("lp([[-1], [-1]], [[3, 1], [1, 3]], [[1], [1]], 0, inf)", Some(30))
    val quarter = _Rational.of(1, 4)
    assert(r.x.forall(v => quarter.contains(v)), r.x)
    assert(_Rational.of(-1, 2).contains(r.objective), r.objective)
  }

  it should "map a variable bounded above only, and name the bounds holding in the active set (F_0113)" in {
    val inf = _Number(Double.PositiveInfinity)
    // max x on x ≤ 3 with no lower bound: the (None, Some(u)) map x = u − y.
    lpSolve(Vector(_Number(-1)), None, None, Vector(_Number(Double.NegativeInfinity)), Vector(_Number(3))) match
      case Right(r) => assert(doubles(r.x) == Vector(3.0) && r.active == Vector(QpConstraint.Upper(0)), r)
      case Left(why) => fail(s"$why")
    // min x on x ≥ −3: the lower bound holds.
    lpSolve(Vector(_Number(1)), None, None, Vector(_Number(-3)), Vector(inf)) match
      case Right(r) => assert(doubles(r.x) == Vector(-3.0) && r.active == Vector(QpConstraint.Lower(0)), r)
      case Left(why) => fail(s"$why")
    // Both bounds finite (the upper one becomes a row, F_0106's match): max x on 1 ≤ x ≤ 2.
    lpSolve(Vector(_Number(-1)), None, None, Vector(_Number(1)), Vector(_Number(2))) match
      case Right(r) => assert(doubles(r.x) == Vector(2.0) && r.active == Vector(QpConstraint.Upper(0)), r)
      case Left(why) => fail(s"$why")
  }

  it should "not cycle on Beale's degenerate example (Bland's rule)" in {
    // min -3/4 x1 + 20 x2 - 1/2 x3 + 6 x4 on 1/4 x1 - 8 x2 - x3 + 9 x4 <= 0,
    // 1/2 x1 - 12 x2 - 1/2 x3 + 3 x4 <= 0, x3 <= 1, x >= 0: optimum -5/4 at (1, 0, 1, 0).
    val r = solved("lp([[-0.75], [20], [-0.5], [6]], [[0.25, -8, -1, 9], [0.5, -12, -0.5, 3], [0, 0, 1, 0]], " +
                   "[[0], [0], [1]], 0, inf)")
    assert(r.objective == _Number(-1.25) && doubles(r.x) == Vector(1.0, 0.0, 1.0, 0.0), r)
  }

  it should "report an infeasible and an unbounded problem as such" in {
    assert(failure("lp(1, [[1], [-1]], [[1], [-2]], -inf, inf)") == LpFailure.Infeasible, "x <= 1 and x >= 2")
    assert(failure("lp(-1, 0, 0, 0, inf)") == LpFailure.Unbounded, "min -x on x >= 0")
    assert(failure("lp(1, 0, 0, 2, 1)") == LpFailure.Infeasible, "lb > ub")
  }

  it should "decline malformed input" in {
    assert(failure("lp([[1], [1]], [[1, 2, 3]], [[1]], 0, inf)").isInstanceOf[LpFailure.InvalidInput])
    assert(failure("lp([[1], [x]], 0, 0, 0, inf)").isInstanceOf[LpFailure.InvalidInput])
  }

  it should "agree with a large random problem's known optimum (the dual certifies it)" in {
    // max sum x on x_i + x_{i+1} <= 1 for a cycle of 7 variables: the optimum is 7/2 at x = 1/2.
    val n = 7
    val rows = (0 until n).map(i => (0 until n).map(j => if j == i || j == (i + 1) % n then "1" else "0").mkString("[", ", ", "]"))
    val src = s"lp(${Vector.fill(n)("[-1]").mkString("[", ", ", "]")}, ${rows.mkString("[", ", ", "]")}, " +
              s"${Vector.fill(n)("[1]").mkString("[", ", ", "]")}, 0, inf)"
    assert(solved(src, Some(30)).objective == _Rational.of(-7, 2).getOrElse(fail("-7/2")))
  }

  it should "measure every coefficient against MaxLpBits, not the pivot row alone (F_0105)" in {
    // min -x - y on x + y <= 1 and H x + H y <= 5H, H = 2^2100. The first pivot is on row 0,
    // whose entries stay one bit wide; it rewrites row 1 to (0, 0, -H, 1 | 4H), past the cap.
    // Measuring the pivot row only, the solver answered on coefficients it had declared too large.
    val h   = _Rational(BigInt(2).pow(2100))
    val one = _Rational(1)
    val r = lpSolve(Vector(_Rational(-1), _Rational(-1)),
                    Some((Vector(Vector(one, one), Vector(h, h)), Vector(one, _Rational(BigInt(5) * BigInt(2).pow(2100))))),
                    None, Vector(_Rational(0), _Rational(0)), Vector(_Number(Double.PositiveInfinity), _Number(Double.PositiveInfinity)))
    r match
      case Left(LpFailure.TooLarge(bits)) => assert(bits > MaxLpBits, bits)
      case other                          => fail(s"expected TooLarge, got $other")
    // The same problem in unit scale is answered: the cap reads coefficients, not the shape.
    assert(doubles(solved("lp([[-1], [-1]], [[1, 1], [1, 1]], [[1], [5]], 0, inf)").x).sum == 1.0)
    // Growth, not input size: every input coefficient is under the cap (K = 2^1000, H = 2^1100),
    // and the pivot row's widest entry is K. Pivoting on x + K y <= 1 rewrites H x + y <= 5H's
    // y-coefficient to 1 - H K, about 2^2100.
    val k = _Rational(BigInt(2).pow(1000))
    val g = _Rational(BigInt(2).pow(1100))
    lpSolve(Vector(_Rational(-1), _Rational(-1)),
            Some((Vector(Vector(one, k), Vector(g, one)), Vector(one, _Rational(BigInt(5) * BigInt(2).pow(1100))))),
            None, Vector(_Rational(0), _Rational(0)), Vector(_Number(Double.PositiveInfinity), _Number(Double.PositiveInfinity))) match
      case Left(LpFailure.TooLarge(bits)) => assert(bits > MaxLpBits, bits)
      case other                          => fail(s"expected TooLarge from a rewritten row, got $other")
  }

  /** F_0105's dense problems: `n` variables, `m` rows of `A x ≤ b` with `A` in `0 … 9` (or, mixed,
   *  `−5 … 9` with `x ≤ 100`), `b` in `50 … 149`, every cost in `−9 … −1`, `x ≥ 0`. */
  private def dense(mixed: Boolean, seed: Int, n: Int, m: Int) =
    val r = new java.util.Random(seed.toLong)
    def num(d: Int): _Value = _Number(d.toDouble)
    val a = Vector.fill(m)(Vector.fill(n)(num(if mixed then r.nextInt(15) - 5 else r.nextInt(10))))
    val b = Vector.fill(m)(num(50 + r.nextInt(100)))
    val f = Vector.fill(n)(num(-1 - r.nextInt(9)))
    (f, Some((a, b)), Vector.fill(n)(num(0)), Vector.fill(n)(if mixed then num(100) else _Number(Double.PositiveInfinity)))

  it should "drive an artificial out on a negative pivot (F_0115)" in {
    // -x + y = 0 and x - y = 0: phase 1 ends at once with both artificials basic at zero, and the
    // first non-zero entry of the first row is -1, so the integer tableau's denominator turns
    // negative and the whole tableau is negated. The second row is then redundant and retired.
    // max y on x <= 3 is (3, 3).
    val one = _Number(1); val minus = _Number(-1); val zero = _Number(0)
    lpSolve(Vector(zero, minus), Some((Vector(Vector(one, zero)), Vector(_Number(3)))),
            Some((Vector(Vector(minus, one), Vector(one, minus)), Vector(zero, zero))),
            Vector(zero, zero), Vector(_Number(Double.PositiveInfinity), _Number(Double.PositiveInfinity))) match
      case Right(r)  => assert(doubles(r.x) == Vector(3.0, 3.0) && r.objective == _Number(-3.0), r)
      case Left(why) => fail(s"$why")
  }

  it should "decline MaxWork past a lowered work budget, and answer under the default (F_0115)" in {
    val (f, a, lb, ub) = dense(mixed = true, seed = 1, n = 20, m = 40)
    lpSolve(f, a, None, lb, ub, Limits.Default.withLimit(Limit.MaxLpWork, 100000)) match
      case Left(LpFailure.MaxWork(budget)) => assert(budget == 100000)
      case other                           => fail(s"expected MaxWork, got $other")
    lpSolve(f, a, None, lb, ub) match
      case Right(r)  => assert(r.iterations == 55, r.iterations)
      case Left(why) => fail(s"$why")
  }

  it should "decline the worst dense 40 x 80 problem promptly at the default budget (F_0115)" in {
    // 352 pivots; in spire rationals it took 139 s on Node, in the integer tableau 7.7 s, and the
    // budget stops it at ~2 s on Node (0.55 s on the JVM). The bound is generous for loaded runs.
    val (f, a, lb, ub) = dense(mixed = true, seed = 4, n = 40, m = 80)
    val t0 = System.nanoTime()
    val res = lpSolve(f, a, None, lb, ub)
    val s = (System.nanoTime() - t0) / 1e9
    assert(res == Left(LpFailure.MaxWork(MaxLpWork)), res)
    assert(s < 10, s"$s s")
  }

  "the lp production" should "answer a column, round-trip, and reserve lp" in {
    val e = parse("lp([[-3], [-5]], [[1, 0], [0, 2], [3, 2]], [[4], [12], [18]], 0, inf)")
    e.eval(env) match
      case Right(m: _MatrixValue) => assert(m.rows == 2 && m.cols == 1 && m.toVector == Vector(2.0, 6.0))
      case other                  => fail(s"$other")
    assert(parse(e.toString) == e)
    assert(Parser.ReservedWords.contains("lp"))
    val bad = parse("lp(1, [[1], [-1]], [[1], [-2]], -inf, inf)")
    assert(bad.eval(env) == Left(bad))
  }
