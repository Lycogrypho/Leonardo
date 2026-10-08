package it.grypho.scala.leonardo
package control

import core.*
import scalar.*
import matrix._Matrix
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0059 — `linearize`: the state-space row of a nonlinear model at an operating point.
 *
 *  Beyond the acceptance pendulum, the suite pins what a linearisation gets wrong quietly: the
 *  derivatives against central finite differences of the model itself (a transposed block or a
 *  swapped tuple would pass a hand-picked case), the offsets at a point that is not an
 *  equilibrium, and the refusals — a point outside the domain, a kink, a malformed request —
 *  each of which must decline with the entry named rather than produce a number.
 */
class LinearizeTest extends AnyFlatSpec:

  private val env = new Environment()
  private def v(n: String) = _Variable(n)

  private def parse(s: String, exact: Option[Int] = None): _Expression =
    Parser.parse(s, exact) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def dense(e: _Expression): _MatrixValue = e match
    case m: _MatrixValue => m
    case other           => fail(s"expected a dense matrix, got $other")

  private def mat(rows: Int, cols: Int, xs: Double*) = _MatrixValue(rows, cols, xs.toArray)

  private def assertClose(a: _MatrixValue, b: _MatrixValue, tol: Double = 1e-12): Unit =
    assert(a.rows == b.rows && a.cols == b.cols, s"shape ${a.rows}x${a.cols} vs ${b.rows}x${b.cols}")
    for i <- 0 until a.rows; j <- 0 until a.cols do
      assert(math.abs(a(i, j) - b(i, j)) <= tol, s"($i,$j): ${a(i, j)} vs ${b(i, j)}")

  private def lin(f: String, g: String, x: Seq[String], u: Seq[String], x0: String, u0: String,
                  exact: Option[Int] = None): Either[LinearizeFailure, Linearization] =
    linearize(parse(f, exact), parse(g, exact), x.map(v).toVector, u.map(v).toVector,
              parse(x0, exact), parse(u0, exact), env)

  private def ok(r: Either[LinearizeFailure, Linearization]): Linearization =
    r.fold(why => fail(s"declined: $why"), identity)

  private val pendulum = ("[[x2], [-sin(x1) + u]]", "[[x1]]", Seq("x1", "x2"), Seq("u"))

  "linearize" should "give the acceptance pendulum's A, B, C, D at the origin" in
  {
    val (f, g, x, u) = pendulum
    val l = ok(lin(f, g, x, u, "[[0], [0]]", "0"))
    assertClose(dense(l.a), mat(2, 2, 0, 1, -1, 0))
    assertClose(dense(l.b), mat(2, 1, 0, 1))
    assertClose(dense(l.c), mat(1, 2, 1, 0))
    assertClose(dense(l.d), mat(1, 1, 0))
    assertClose(dense(l.f0), mat(2, 1, 0, 0))         // the origin is an equilibrium
  }

  it should "produce a row c2dExact, controllable and observable accept unchanged" in
  {
    val (f, g, x, u) = pendulum
    val l = ok(lin(f, g, x, u, "[[0], [0]]", "0"))
    l.model match
      case _Matrix(1, 4, Vector(a, b, c, _)) =>
        assert(controllable(a, b).contains(true))
        assert(observable(a, c).contains(true))
        assert(c2dExact(a, b, 0.1).isDefined)
      case other => fail(s"expected the 1x4 row, got $other")
  }

  it should "carry the offsets F0, G0 at a point that is not an equilibrium" in
  {
    val (f, g, x, u) = pendulum
    val l = ok(lin(f, g, x, u, "[[pi/2], [3]]", "0.5"))
    assertClose(dense(l.a), mat(2, 2, 0, 1, -math.cos(math.Pi / 2), 0))
    assertClose(dense(l.f0), mat(2, 1, 3, -1 + 0.5))
    assertClose(dense(l.g0), mat(1, 1, math.Pi / 2))
    l.affine match
      case m: _Matrix => assert(m.rows == 1 && m.cols == 6)
      case other      => fail(s"$other")
  }

  it should "agree with central finite differences of the model, block by block" in
  {
    // Three states, two inputs, every block non-trivial: a transposed block or a swapped tuple
    // would agree on a hand-picked case and fail here.
    val f = "[[x1*x2 + u1], [exp(-x3)*u2 - x1^2], [sin(x2) + x3*u1*u2]]"
    val g = "[[x1 + x3*u2], [cos(x1*x2)]]"
    val xs = Vector("x1", "x2", "x3"); val us = Vector("u1", "u2")
    val p  = Vector(0.3, -0.7, 1.1, 0.4, -1.3)
    val l  = ok(lin(f, g, xs, us, s"[[${p(0)}], [${p(1)}], [${p(2)}]]", s"[[${p(3)}], [${p(4)}]]"))
    val names = xs ++ us
    def eval(field: String, q: Vector[Double]): Vector[Double] =
      val e = names.zip(q).foldLeft(env)((en, kv) => en.withBinding(kv._1, _Number(kv._2)))
      parse(field).eval(e) match
        case Right(m: _MatrixValue) => m.toVector
        case other                  => fail(s"$other")
    def fd(field: String, cols: Range): _MatrixValue =
      val h = 1e-6
      val diffs = cols.map { k =>
        val (up, dn) = (p.updated(k, p(k) + h), p.updated(k, p(k) - h))
        eval(field, up).zip(eval(field, dn)).map((a, b) => (a - b) / (2 * h))
      }
      val rows = diffs.head.size
      _MatrixValue(rows, cols.size, Array.tabulate(rows * cols.size)(i => diffs(i % cols.size)(i / cols.size)))
    assertClose(dense(l.a), fd(f, 0 until 3), 1e-7)
    assertClose(dense(l.b), fd(f, 3 until 5), 1e-7)
    assertClose(dense(l.c), fd(g, 0 until 3), 1e-7)
    assertClose(dense(l.d), fd(g, 3 until 5), 1e-7)
  }

  it should "linearise symbolically about a point with a free parameter" in
  {
    val (f, g, x, u) = pendulum
    val l = ok(lin(f, g, x, u, "[[a], [0]]", "0"))
    l.a match
      case m: _Matrix =>
        assert(m.elems(2).freeVars == Set("a"), s"A(2,1) = ${m.elems(2)}")
        val at = m.elems(2).eval(env.withBinding("a", _Number(0.4)))
        assert(at match { case Right(_Number(d)) => math.abs(d + math.cos(0.4)) < 1e-12; case _ => false }, s"$at")
      case other => fail(s"expected a symbolic A, got $other")
  }

  it should "stay exact on an exact polynomial model at an exact point" in
  {
    val l = ok(lin("[[x1^2 + x2/3 + u]]", "[[x1*u]]", Seq("x1", "x2"), Seq("u"), "[[1/2], [1]]", "2", exact = Some(30)))
    l.a match
      case m: _Matrix =>
        val third = parse("1/3", Some(30)).eval(env).toExpression
        assert(m.elems == Vector(_Rational(1), third) && third.isInstanceOf[_Rational], s"$m")
      case other      => fail(s"expected exact cells, got $other")
  }

  it should "accept a bare expression as a one-component field or point" in
  {
    val l = ok(lin("-x + u", "x", Seq("x"), Seq("u"), "1", "0"))
    assertClose(dense(l.a), mat(1, 1, -1))
    assertClose(dense(l.f0), mat(1, 1, -1))
  }

  // --- refusals ---

  it should "decline a point outside the domain, naming the component" in
  {
    lin("[[ln(x1) + u]]", "[[x1]]", Seq("x1"), Seq("u"), "-1", "0") match
      case Left(LinearizeFailure.OutsideDomain(what, vv, _)) => assert(what == "F row 1" && vv == v("x1"))
      case other => fail(s"expected OutsideDomain, got $other")
  }

  it should "decline at a kink, where the derivative is not a number" in
  {
    lin("[[abs(x1) + u]]", "[[x1]]", Seq("x1"), Seq("u"), "0", "0") match
      case Left(LinearizeFailure.NotReal(what)) => assert(what == "A(1, 1)", what)
      case other => fail(s"expected NotReal, got $other")
  }

  it should "decline a malformed request with a reason" in
  {
    val (f, g, _, _) = pendulum
    def invalid(r: Either[LinearizeFailure, Linearization]) =
      assert(r match { case Left(LinearizeFailure.InvalidInput(_)) => true; case _ => false }, s"$r")
    invalid(lin(f, g, Seq("x1", "x2"), Seq("x1"), "[[0], [0]]", "0"))       // a name twice
    invalid(lin(f, g, Seq("x1", "x2"), Seq("u"), "[[0], [0], [0]]", "0"))  // wrong length
    invalid(lin(f, g, Seq("x1", "x2"), Seq("u"), "[[0, 0]]", "0"))         // a row, not a column
    invalid(lin("[[x2, x1]]", g, Seq("x1", "x2"), Seq("u"), "[[0], [0]]", "0"))
    invalid(lin(f, g, Seq("x1", "x2"), Seq("u"), "[[u], [0]]", "0"))       // the point names u
  }

  // --- the node ---

  "the linearize node" should "evaluate to the row, round-trip, and leave a refusal as written" in
  {
    val call = parse("linearize([[x2], [-sin(x1) + u]], [[x1]], [[x1], [x2]], [[u]], [[0], [0]], 0)")
    call.eval(env) match
      case Left(_Matrix(1, 4, _)) => succeed
      case other                  => fail(s"$other")
    parse("linearize([[x2], [-sin(x1) + u]], [[x1]], [[x1], [x2]], u, [[0], [0]], 0, affine)").eval(env) match
      case Left(_Matrix(1, 6, _)) => succeed
      case other                  => fail(s"$other")
    for s <- List("linearize(F, G, [[x1], [x2]], [[u]], p, q)", "linearize(F, G, x, u, p, q, affine)") do
      val e = parse(s)
      assert(parse(e.toString) == e, s"$s -> $e")
    val bad = parse("linearize([[ln(x1) + u]], [[x1]], x1, u, -1, 0)")
    assert(bad.eval(env) == Left(bad))
    assert(Parser.ReservedWords.contains("linearize") && !Parser.ReservedWords.contains("affine"))
  }
