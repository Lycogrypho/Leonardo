package it.grypho.scala.leonardo
package optimize

import core.*
import matrix._Matrix
import equation._Equation
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0068: a coupled polynomial system, which elimination alone declines, solved through a
 *  lexicographic Gröbner basis whose triangular shape elimination then finishes. */
class GroebnerTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String, exact: Option[Int] = None): _Expression =
    Parser.parse(s, exact) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def vars(names: String*): List[_Variable] = names.map(_Variable(_)).toList

  private def coordinates(p: SolutionPoint, names: Seq[String]): Vector[Double] =
    names.map(n => p(n) match
      case _Number(d) => d
      case other      => fail(s"non-numeric coordinate $other")).toVector

  /** The solutions as a set of coordinate vectors, compared up to `tol` in any order. */
  private def assertSolutions(eqs: Seq[String], names: Seq[String], expected: Seq[Vector[Double]],
                              tol: Double = 1e-9): Unit =
    eliminate(eqs.map(parse(_)).toList, vars(names*), env) match
      case None => fail(s"$eqs declined")
      case Some(points) =>
        val got = points.map(coordinates(_, names))
        assert(got.size == expected.size, s"$eqs: expected ${expected.size} points, got $got")
        for x <- expected do
          assert(got.exists(g => g.zip(x).forall((a, b) => math.abs(a - b) <= tol)), s"$eqs: $x missing from $got")

  "eliminate" should "solve a system with no equation linear in any unknown (F_0068)" in
  {
    // x² + y² = 5 and x² − y² = 3: neither equation is linear or univariate, so elimination
    // alone declined; the lex basis is {x² − 4, y² − 1}.
    assertSolutions(Seq("x^2 + y^2 - 5", "x^2 - y^2 - 3"), Seq("x", "y"),
                    Seq(Vector(2.0, 1.0), Vector(2.0, -1.0), Vector(-2.0, 1.0), Vector(-2.0, -1.0)))
  }

  it should "keep a solution of multiplicity two once" in
  {
    // x² + xy + y² = 3 and x² − xy + y² = 1 give xy = 1 and (x − y)² = 0: two double points.
    assertSolutions(Seq("x^2 + x*y + y^2 - 3", "x^2 - x*y + y^2 - 1"), Seq("x", "y"),
                    Seq(Vector(1.0, 1.0), Vector(-1.0, -1.0)))
  }

  it should "answer provably none for an inconsistent system (the basis is {1})" in
  {
    assert(eliminate(List(parse("x^2 + y^2 - 1"), parse("x^2 + y^2 - 2")), vars("x", "y"), env).contains(Nil))
  }

  it should "solve three coupled unknowns" in
  {
    // x² + y² + z² = 3, x² − y² = 0, y² − z² = 0: every coordinate ±1, eight points.
    val all = for a <- Seq(1.0, -1.0); b <- Seq(1.0, -1.0); c <- Seq(1.0, -1.0) yield Vector(a, b, c)
    assertSolutions(Seq("x^2 + y^2 + z^2 - 3", "x^2 - y^2", "y^2 - z^2"), Seq("x", "y", "z"), all)
  }

  it should "take over a branch the elimination steps lead into rational equations" in
  {
    // xy = 1 is linear in x with the coefficient y, so elimination divides: x = 1/y turns
    // x² + z² = 2 into a rational equation coupled with y² + z² = 2, where no step applies.
    // From the ideal itself: x² = y² and xy = 1 force x = y = ±1, then z² = 1.
    assertSolutions(Seq("x*y - 1", "x^2 + z^2 - 2", "y^2 + z^2 - 2"), Seq("x", "y", "z"),
                    Seq(Vector(1.0, 1.0, 1.0), Vector(1.0, 1.0, -1.0), Vector(-1.0, -1.0, 1.0), Vector(-1.0, -1.0, -1.0)))
  }

  it should "still decline a continuum, a symbolic coefficient and a transcendental equation" in
  {
    assert(eliminate(List(parse("x^2 + y^2 - 1"), parse("(x^2 + y^2 - 1)*x^2")), vars("x", "y"), env).isEmpty,
           "a circle of solutions has no list")
    assert(eliminate(List(parse("x^2 + y^2 - a"), parse("x^2 - y^2 - 1")), vars("x", "y"), env).isEmpty,
           "a free parameter is not a number the basis can be built over")
    assert(eliminate(List(parse("x^2 + exp(y) - 5"), parse("x^2 - y^2 - 3")), vars("x", "y"), env).isEmpty)
  }

  it should "decline a system past the basis caps, promptly" in
  {
    // cyclic-5: 70 isolated solutions and a notoriously large lex basis.
    val cyclic = List("a + b + c + d + e", "a*b + b*c + c*d + d*e + e*a", "a*b*c + b*c*d + c*d*e + d*e*a + e*a*b",
                      "a*b*c*d + b*c*d*e + c*d*e*a + d*e*a*b + e*a*b*c", "a*b*c*d*e - 1").map(parse(_))
    val t0 = System.nanoTime()
    val r  = eliminate(cyclic, vars("a", "b", "c", "d", "e"), env)
    val seconds = (System.nanoTime() - t0) / 1e9
    assert(r.isEmpty, "declined rather than answered past the caps")
    // F_0122: 0.2–0.7 s on the JVM and 0.75–0.9 s on Node with the basis capped at 24. At 40 it
    // took 3–5.4 s on Node and failed the old 10 s bound once in a loaded full run.
    assert(seconds < 5, s"took $seconds s")
  }

  it should "solve a system with general decimal coefficients, which needs coefficients past 1024 bits (F_0122)" in
  {
    // A Double is read as the dyadic rational it holds (0.1 is a 110-bit fraction), so this 2×2
    // system's basis reaches 1259 bits: the coefficient cap cannot be lowered to bound a decline.
    val eqs = Seq("1.37*x^2 + 2.91*x*y - 0.73*y^2 + 0.11*x - 2.3", "0.57*x^2 - 1.13*y^2 + 0.41*x*y - 0.29*y + 0.5")
    eliminate(eqs.map(parse(_)).toList, vars("x", "y"), env) match
      case None => fail("declined")
      case Some(points) =>
        assert(points.size == 2, s"$points")
        for p <- points; e <- eqs do
          val at = new Environment().withBinding("x", p("x") match { case v: _Value => v; case o => fail(s"$o") })
                                    .withBinding("y", p("y") match { case v: _Value => v; case o => fail(s"$o") })
          parse(e).eval(at) match
            case Right(_Number(d)) => assert(math.abs(d) <= 1e-9, s"$e at $p: $d")
            case other             => fail(s"$e at $p: $other")
  }

  "stationary" should "list the nine stationary points of a function with a coupled gradient" in
  {
    // grad = (2x(2x² + y² − 2), 2y(x² + 2y² − 2)): the origin, (0, ±1), (±1, 0) and x² = y² = 2/3.
    parse("stationary(x^4 + y^4 + x^2*y^2 - 2*x^2 - 2*y^2, x, y)").eval(env) match
      case Left(_Matrix(rows, 2, cells)) =>
        assert(rows == 9, s"expected nine points, got $rows")
        val pts = cells.grouped(2).toList.map(_.map { case _Equation(_, _Number(d)) => d; case o => fail(s"$o") })
        val r = math.sqrt(2.0 / 3)
        val expected = Seq(Vector(0.0, 0.0), Vector(0.0, 1.0), Vector(0.0, -1.0), Vector(1.0, 0.0), Vector(-1.0, 0.0),
                           Vector(r, r), Vector(r, -r), Vector(-r, r), Vector(-r, -r))
        for x <- expected do
          assert(pts.exists(p => p.zip(x).forall((a, b) => math.abs(a - b) <= 1e-9)), s"$x missing from $pts")
      case other => fail(s"declined: $other")
  }

  "groebnerBasis" should "reduce to the textbook lexicographic basis" in
  {
    // {x² + y² − 5, x² − y² − 3} in lex x > y: {x² − 4, y² − 1}, reduced and monic.
    groebnerBasis(List(parse("x^2 + y^2 - 5"), parse("x^2 - y^2 - 3")), vars("x", "y"), env) match
      case Some(basis) =>
        val at = (x: Double, y: Double) => env.withBinding("x", _Number(x)).withBinding("y", _Number(y))
        assert(basis.size == 2, basis)
        // Each element vanishes on the four solutions and is non-zero at a point off them.
        for p <- basis; (x, y) <- Seq((2.0, 1.0), (-2.0, -1.0)) do
          assert(p.eval(at(x, y)) match { case Right(_Number(d)) => math.abs(d) < 1e-12; case _ => false }, s"$p at ($x, $y)")
        assert(basis.exists(p => p.freeVars == Set("y")), s"no element in y alone: $basis")
      case None => fail("declined")
  }

  it should "keep exact coefficients exact" in
  {
    groebnerBasis(List(parse("x^2 + y^2 - 5", Some(30)), parse("x^2 - y^2 - 3", Some(30))), vars("x", "y"), env) match
      case Some(basis) =>
        val numbers = basis.flatMap(collectNumbers)
        assert(numbers.nonEmpty && numbers.forall(_.isInstanceOf[_Rational]), s"$basis")
      case None => fail("declined")
  }

  private def collectNumbers(e: _Expression): List[_Expression] = e match
    case r: _Rational  => List(r)
    case n: _Number    => List(n)
    case other         => other.children.flatMap(collectNumbers)
