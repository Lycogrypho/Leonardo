package it.grypho.scala.leonardo
package optimize

import core.*
import matrix._Matrix
import equation._Equation
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

import scala.math.abs


/** F_0010 — optimization: stationary points, convexity, Lagrange, KKT and `minimize`.
 *
 *  The acceptance values are the plan's (and MPC requirement T2-06's) verbatim.  Points are
 *  compared numerically, never through `toString`.
 */
class OptimizeTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String, exact: Option[Int] = None): _Expression =
    Parser.parse(s, exact) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  /** The points of a `stationary` / `lagrange` answer as coordinate vectors. */
  private def points(s: String, e: Environment = env): List[Vector[Double]] =
    parse(s).eval(e) match
      case Left(_Matrix(rows, cols, cells)) =>
        cells.grouped(cols).toList.map(_.map {
          case _Equation(_, _Number(d)) => d
          case other                    => fail(s"non-numeric coordinate $other in \"$s\"")
        })
      case other => fail(s"\"$s\" gave no points: $other")

  private def close(a: Vector[Double], b: Vector[Double], tol: Double = 1e-9): Boolean =
    a.size == b.size && a.zip(b).forall((x, y) => abs(x - y) <= tol)

  private def assertPoints(s: String, expected: Vector[Double]*): Unit =
    val got = points(s)
    assert(got.size == expected.size, s"\"$s\": expected ${expected.size} points, got $got")
    for (g, x) <- got.zip(expected) do assert(close(g, x), s"\"$s\": got $g, expected $x")

  private def symbolic(s: String): Boolean = parse(s).eval(env).isLeft &&
    (parse(s).eval(env) match
      case Left(_: _Matrix) => false
      case _                => true)

  private def vars(names: String*): Vector[_Variable] = names.map(_Variable(_)).toVector

  // --- slice 1: stationary points ---

  "stationary" should "solve a linear gradient system" in
  {
    assertPoints("stationary(x^2 + x*y + y^2 - 3*x, x, y)", Vector(2.0, -1.0))
  }

  "eliminate" should "stay exact on exact input" in
  {
    // The gradient of x^2 + x*y + y^2 - 3*x, written out: `derive` itself demotes an exact
    // power's coefficient to a Double (filed separately), so the exactness of the SOLVER is
    // pinned on equations that are exact as given.
    val eqs = List(parse("2*x + y - 3", Some(30)), parse("x + 2*y", Some(30)))
    eliminate(eqs, vars("x", "y").toList, env) match
      case Some(List(p)) =>
        (p("x"), p("y")) match
          case (a: _Rational, b: _Rational) => assert(a.toDouble == 2.0 && b.toDouble == -1.0)
          case other                        => fail(s"expected exact coordinates, got $other")
      case other => fail(s"expected one exact point, got $other")
  }

  it should "solve a decoupled nonlinear system (elimination, Decision B)" in
  {
    assertPoints("stationary(x^3 - 3*x + y^2, x, y)", Vector(-1.0, 0.0), Vector(1.0, 0.0))
  }

  it should "keep the points where an eliminated coefficient vanishes" in
  {
    // grad = (y(2x + y - 3), x(x + 2y - 3)).  Solving the first for x divides by 2y; the
    // branch y = 0 is what finds (0, 0) and (3, 0).  Without it two of the four are lost
    // and the list still LOOKS complete.
    assertPoints("stationary(x*y*(x + y - 3), x, y)",
                 Vector(0.0, 0.0), Vector(0.0, 3.0), Vector(1.0, 1.0), Vector(3.0, 0.0))
  }

  it should "answer false when there are provably none" in
  {
    assert(parse("stationary(x + y, x, y)").eval(env) == Right(_Bool(false)))
    assert(parse("stationary(exp(x), x)").eval(env).isLeft)   // transcendental: declined, see below
  }

  it should "decline a continuum and a transcendental gradient rather than list part of it" in
  {
    assert(symbolic("stationary((x - y)^2, x, y)"))   // the whole line x = y
    assert(symbolic("stationary(sin(x), x)"))         // infinitely many
    assert(symbolic("stationary(x^2, x, x)"))         // no basis
  }

  it should "treat a bound parameter as a number and leave an unbound one symbolic" in
  {
    val bound = env.withBinding("a", _Number(4.0))
    assert(close(points("stationary(x^2 - a*x, x)", bound).head, Vector(2.0)))
    parse("stationary(x^2 - a*x, x)").eval(env) match
      case Left(_Matrix(1, 1, Vector(_Equation(_, value)))) =>
        assert(value.freeVars == Set("a"), s"expected x = a/2, got $value")
      case other => fail(s"expected a symbolic point, got $other")
  }

  "classifyStationary" should "read the Hessian's signs" in
  {
    val f = parse("x^3 - 3*x + y^2")
    val xy = vars("x", "y")
    assert(classifyStationary(f, xy, Map("x" -> 1.0, "y" -> 0.0), env).contains(StationaryKind.Minimum))
    assert(classifyStationary(f, xy, Map("x" -> -1.0, "y" -> 0.0), env).contains(StationaryKind.Saddle))
    // Written `0 - ...`: a LEADING minus negates the whole sum in the current grammar (filed
    // as a Priority 1 parser defect), so `-(x^2) - y^2` would be read as a saddle.
    assert(classifyStationary(parse("0 - x^2 - y^2"), xy, Map("x" -> 0.0, "y" -> 0.0), env)
             .contains(StationaryKind.Maximum))
    // x^4: zero Hessian at a minimum -- the test cannot decide, and says so
    assert(classifyStationary(parse("x^4"), vars("x"), Map("x" -> 0.0), env)
             .contains(StationaryKind.Degenerate))
  }

  it should "not move when the function is rescaled" in
  {
    val pt = Map("x" -> 1.0, "y" -> 0.0)
    for k <- List("1e-6", "1", "1e6") do
      assert(classifyStationary(parse(s"$k * (x^3 - 3*x + y^2)"), vars("x", "y"), pt, env)
               .contains(StationaryKind.Minimum), s"scale $k")
  }

  // --- slice 2: convexity ---

  "convex" should "prove convexity from the principal minors" in
  {
    assert(parse("convex(x^2 + y^2, x, y)").eval(env) == Right(_Bool(true)))
    assert(parse("convex(x^4 + y^2, x, y)").eval(env) == Right(_Bool(true)))  // semidefinite at 0
    assert(parse("convex(exp(x), x)").eval(env) == Right(_Bool(true)))
  }

  it should "refute convexity only with a witness" in
  {
    assert(parse("convex(x*y, x, y)").eval(env) == Right(_Bool(false)))
    assert(parse("convex(x^3, x)").eval(env) == Right(_Bool(false)))
  }

  it should "read every principal minor, not only the leading ones" in
  {
    // H = diag(0, -2): leading minors 0 and 0 look semidefinite; the minor -2 is not.
    assert(parse("convex(-(y^2), x, y)").eval(env) == Right(_Bool(false)))
  }

  it should "answer true only on a domain where the claim makes sense" in
  {
    // 1/x^2 has a positive Hessian wherever it exists, but x != 0 is not a convex domain.
    assert(parse("convex(1/x^2, x)").eval(env).isLeft)
    // x ln x is negatively curved at x = -1, where it is not real: no witness there.
    assert(parse("convex(x*ln(x), x)").eval(env).isLeft)
  }

  // --- slice 3: Lagrange ---

  "lagrange" should "find the constrained stationary points" in
  {
    assertPoints("lagrange(x + y, [[x^2 + y^2 - 2]], x, y)", Vector(-1.0, -1.0), Vector(1.0, 1.0))
    assertPoints("lagrange(x + y, [[x^2 + y^2 = 2]], x, y)", Vector(-1.0, -1.0), Vector(1.0, 1.0))
    assertPoints("lagrange(x + y, x^2 + y^2 = 2, x, y)", Vector(-1.0, -1.0), Vector(1.0, 1.0))
  }

  it should "handle several constraints and a linear problem exactly" in
  {
    // min x^2 + y^2 + z^2 on x + y + z = 3 and x - y = 0: the point (1, 1, 1).
    assertPoints("lagrange(x^2 + y^2 + z^2, [[x + y + z - 3], [x - y]], x, y, z)", Vector(1.0, 1.0, 1.0))
  }

  it should "refuse an inequality as a Lagrange constraint" in
  {
    assert(symbolic("lagrange(x + y, [[x^2 + y^2 <= 2]], x, y)"))
  }

  // --- slice 4: KKT ---

  "kkt" should "state conditions that hold exactly at the KKT point" in
  {
    // min x^2 + y^2 s.t. 1 - x - y <= 0: the point (1/2, 1/2) with mu0 = 1.
    val conditions = parse("kkt(x^2 + y^2, 1 - x - y, 0, x, y)").eval(env) match
      case Left(c) => c
      case other   => fail(s"expected the conditions, got $other")
    assert(conditions.freeVars == Set("x", "y", "mu0"))
    def at(x: Double, y: Double, mu: Double) =
      conditions.eval(env.withBinding("x", _Number(x)).withBinding("y", _Number(y)).withBinding("mu0", _Number(mu)))
    assert(at(0.5, 0.5, 1.0) == Right(_Bool(true)))
    assert(at(0.0, 0.0, 0.0) == Right(_Bool(false)))   // infeasible
    assert(at(0.5, 0.5, -1.0) == Right(_Bool(false)))  // a negative multiplier
  }

  it should "avoid capturing a name the problem already uses" in
  {
    parse("kkt(x^2 + mu0, 1 - x, 0, x)").eval(env) match
      case Left(c) => assert(c.freeVars.contains("mu1") && !c.toString.contains("mu0 >= 0"))
      case other   => fail(s"expected the conditions, got $other")
  }

  it should "refuse a strict inequality rather than relax it" in
  {
    parse("kkt(x^2, [[x < 1]], 0, x)").eval(env) match
      case Left(_: _KKT) => succeed
      case other         => fail(s"a strict inequality must not be read as non-strict: $other")
  }

  "kktPoints" should "solve by active sets" in
  {
    val xy = vars("x", "y")
    def numeric(ps: Option[List[SolutionPoint]]): List[Vector[Double]] =
      ps.getOrElse(fail("declined")).map(p => xy.map(v => p(v.variable) match
        case _Number(d) => d
        case other      => fail(s"non-numeric $other")))
    // the constraint is active
    val active = numeric(kktPoints(parse("x^2 + y^2"), parse("1 - x - y"), parse("0"), xy, env))
    assert(active.size == 1 && close(active.head, Vector(0.5, 0.5)))
    // the constraint is inactive: the unconstrained minimum is feasible
    val inactive = numeric(kktPoints(parse("(x - 1)^2 + (y - 1)^2"), parse("x + y - 3"), parse("0"), xy, env))
    assert(inactive.size == 1 && close(inactive.head, Vector(1.0, 1.0)))
  }

  // --- slice 5: minimize ---

  private def minimized(s: String): Vector[Double] = parse(s).eval(env) match
    case Right(m: _MatrixValue) if m.cols == 1 => m.toVector
    case other                                => fail(s"\"$s\" did not minimise: $other")

  private val rosenbrock = "(1 - x)^2 + 100*(y - x^2)^2"

  "minimize" should "reach Rosenbrock's minimum with BFGS" in
  {
    assert(close(minimized(s"minimize($rosenbrock, [[x], [y]], [[-1.2], [1]], bfgs)"), Vector(1.0, 1.0), 1e-6))
  }

  it should "respect a bound with projected BFGS" in
  {
    assert(close(minimized(s"minimize($rosenbrock, [[x], [y]], [[-1.2], [1]], [[-inf], [-inf]], [[0.5], [inf]], pbfgs)"),
                 Vector(0.5, 0.25), 1e-6))
  }

  it should "converge in one Newton step on a convex quadratic" in
  {
    val f = parse("(x - 3)^2 + 2*(y + 1)^2 + x*y")
    minimize(f, vars("x", "y"), Vector(10.0, -7.0), None, MinimizeMethod.Newton, env) match
      case Right(r) => assert(r.iterations == 1, s"took ${r.iterations} iterations")
      case Left(why) => fail(s"declined: $why")
  }

  it should "minimise a well-conditioned quadratic with gradient descent" in
  {
    assert(close(minimized("minimize((x - 1)^2 + (y + 2)^2, [[x], [y]], [[0], [0]], gd)"), Vector(1.0, -2.0), 1e-6))
  }

  it should "decline, with the reason, rather than return an uncertified point" in
  {
    val xy = vars("x", "y")
    def why(f: String, m: MinimizeMethod, b: Option[(Vector[Double], Vector[Double])] = None) =
      minimize(parse(f), xy, Vector(0.5, 0.5), b, m, env)
    assert(why("x + y", MinimizeMethod.BFGS).left.exists(_ == MinimizeFailure.Unbounded))
    assert(why("x^2 - y^2", MinimizeMethod.Newton).left.exists {
      case MinimizeFailure.HessianNotPositiveDefinite(_) => true
      case _                                            => false })
    assert(why("x^2 + y^2", MinimizeMethod.Newton, Some((Vector(0.0, 0.0), Vector(1.0, 1.0))))
             .left.exists(_ == MinimizeFailure.MethodTakesNoBounds(MinimizeMethod.Newton)))
    assert(why("x^2 + y^2", MinimizeMethod.ProjectedBFGS, Some((Vector(1.0, 0.0), Vector(0.0, 1.0))))
             .left.exists(_.isInstanceOf[MinimizeFailure.InvalidInput]))
    assert(why("ln(x) + y^2", MinimizeMethod.BFGS).isLeft)          // leaves the domain
    assert(parse(s"minimize($rosenbrock, [[x], [y]], [[-1.2], [1]], gd)").eval(env).isLeft)
  }

  // --- the language ---

  "the optimization nodes" should "round-trip through toString" in
  {
    for s <- List("stationary(x^2 + y^2, x, y)", "convex(x*y, x, y)",
                  "lagrange(x + y, [[x^2 + y^2 - 2]], x, y)", "kkt(x^2, [[1 - x]], 0, x)",
                  s"minimize($rosenbrock, [[x], [y]], [[-1.2], [1]], bfgs)",
                  s"minimize($rosenbrock, [[x], [y]], [[-1.2], [1]], [[0], [0]], [[2], [2]], pbfgs)") do
      val e = parse(s)
      assert(parse(e.toString) == e, s"round-trip failed for $s: ${e.toString}")
  }

  it should "reserve exactly the function names, not the method words" in
  {
    for w <- List("stationary", "convex", "lagrange", "kkt", "minimize") do
      assert(Parser.ReservedWords.contains(w), s"'$w' must be reserved")
    for w <- List("gd", "newton", "bfgs", "pbfgs") do
      assert(!Parser.ReservedWords.contains(w), s"'$w' must stay a legal variable name")
      assert(parse(s"$w + 1").freeVars == Set(w))
  }
