package it.grypho.scala.leonardo
package control

import core.*
import matrix._Matrix
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0064: discrete-time state-space simulation, `x[k+1] = A·x[k] + B·u[k]`,
 *  `y[k] = C·x[k] + D·u[k]`. Checked against an explicit recursion written here, and against the
 *  continuous step response sampled at the hold instants — the identity tying the discrete and
 *  continuous halves of `control` together. */
class DlsimTest extends AnyFlatSpec:

  private val env = new Environment()

  private def m(rows: Int, cols: Int, xs: Double*) = _MatrixValue(rows, cols, xs.toArray)
  private def close(a: Double, b: Double, tol: Double = 1e-9) = math.abs(a - b) <= tol * math.max(1.0, math.abs(b))

  private def parse(src: String): _Expression =
    val r = Parser.parse(src)
    assert(r.successful, s"parse failed for \"$src\": $r")
    r.get

  /** The recursion, written out with plain loops, independent of the implementation. */
  private def byHand(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, d: _MatrixValue,
                     u: _MatrixValue, x0: Vector[Double]): (Vector[Vector[Double]], Vector[Vector[Double]]) =
    val (n, mm, p, nn) = (a.rows, b.cols, c.rows, u.cols)
    (0 until nn).foldLeft((Vector.empty[Vector[Double]], Vector(x0))) { case ((ys, xs), k) =>
      val x  = xs.last
      val yk = Vector.tabulate(p)(i => (0 until n).map(j => c(i, j) * x(j)).sum + (0 until mm).map(l => d(i, l) * u(l, k)).sum)
      val xn = Vector.tabulate(n)(i => (0 until n).map(j => a(i, j) * x(j)).sum + (0 until mm).map(l => b(i, l) * u(l, k)).sum)
      (ys :+ yk, xs :+ xn)
    }

  private val a = m(2, 2, 0.5, 0.1, -0.2, 0.8)
  private val b = m(2, 2, 1.0, 0.0, 0.3, -1.0)
  private val c = m(2, 2, 1.0, 2.0, 0.0, 1.0)
  private val d = m(2, 2, 0.1, 0.0, 0.0, 0.2)
  private val u = m(2, 30, Vector.tabulate(60)(i => math.sin(0.4 * i) + (i % 7) / 7.0)*)

  "dlsim" should "agree with the explicit recursion, outputs and states" in
  {
    val x0 = Vector(1.0, -0.5)
    val (ys, xs) = byHand(a, b, c, d, u, x0)
    dlsim(a, b, c, d, u, m(2, 1, x0*)) match
      case Left(f) => fail(s"declined: $f")
      case Right((y, x)) =>
        assert(y.rows == 2 && y.cols == 30 && x.rows == 2 && x.cols == 31)
        for k <- 0 until 30; i <- 0 until 2 do assert(close(y(i, k), ys(k)(i)), s"y($i, $k)")
        for k <- 0 to 30; i <- 0 until 2 do assert(close(x(i, k), xs(k)(i)), s"x($i, $k)")
  }

  it should "match the continuous step response at the sample instants, through the zero-order hold" in
  {
    // G(s) = 1/(s^2 + 3s + 2): x' = [[0, 1], [-2, -3]] x + [[0], [1]] u, y = [[1, 0]] x.
    val ts = 0.1
    val (ad, bd) = c2dExact(parse("[[0, 1], [-2, -3]]"), parse("[[0], [1]]"), ts).getOrElse(fail("c2dExact declined"))
    val steps = m(1, 50, Vector.fill(50)(1.0)*)
    val (y, _) = dlsim(ad, bd, m(1, 2, 1, 0), m(1, 1, 0), steps, m(2, 1, 0, 0)).fold(f => fail(s"$f"), identity)
    val response = stepResponse(parse("1/(s^2 + 3*s + 2)"), _Variable("s"), _Variable("t"))
    for k <- 0 until 50 do
      response.eval(env.withBinding("t", _Number(k * ts))) match
        case Right(_Number(expected)) => assert(close(y(0, k), expected), s"k = $k: ${y(0, k)} vs $expected")
        case other                    => fail(s"step response at k = $k: $other")
  }

  it should "decline inconsistent shapes and an overflowing simulation" in
  {
    val x0 = m(2, 1, 0, 0)
    assert(dlsim(a, m(3, 2, 1, 0, 0, 1, 1, 1), c, d, u, x0).left.exists(_.isInstanceOf[SimFailure.ShapeMismatch]), "B rows")
    assert(dlsim(a, b, c, d, m(3, 30, Vector.fill(90)(0.0)*), x0).left.exists(_.isInstanceOf[SimFailure.ShapeMismatch]), "u rows")
    assert(dlsim(a, b, c, d, u, m(3, 1, 0, 0, 0)).left.exists(_.isInstanceOf[SimFailure.ShapeMismatch]), "x0 length")
    assert(dlsim(m(1, 1, 1e100), m(1, 1, 1), m(1, 1, 1), m(1, 1, 0), m(1, 10, Vector.fill(10)(1.0)*), m(1, 1, 1))
      .left.exists(_.isInstanceOf[SimFailure.Overflow]), "a growing state overflows rather than reporting Infinity")
  }

  "the dlsim node" should "read the stateSpace row, 0 for a zero x0, a number as 1x1, and u either way up" in
  {
    val row = "stateSpace([[0.5]], [[1]], [[1]], 0)"
    parse(s"dlsim($row, [[1, 1, 1]], 0)").eval(env) match
      case Right(y: _MatrixValue) => assert(y.toVector == Vector(0.0, 1.0, 1.5))
      case other                  => fail(s"unexpected: $other")
    assert(parse(s"dlsim($row, [[1], [1], [1]], 0)").eval(env) == parse(s"dlsim($row, [[1, 1, 1]], 0)").eval(env))
    parse(s"dlsim($row, [[1, 1, 1]], 2, states)").eval(env) match
      case Left(r: _Matrix) =>
        assert(r.rows == 1 && r.cols == 2)
        r.elems(1) match
          case x: _MatrixValue => assert(x.toVector == Vector(2.0, 2.0, 2.0, 2.0), "x0 = 2 then 0.5x + 1 = 2 forever")
          case other           => fail(s"X: $other")
      case other => fail(s"unexpected: $other")
  }

  it should "simulate arx2ss's realisation exactly as the ARX recursion runs" in
  {
    // y[k] = 0.8 y[k-1] + 0.5 u[k-1] from rest, u = 1, 2, 3, 4.
    parse("dlsim(arx2ss([[0.8]], [[0.5]], 1), [[1, 2, 3, 4]], 0)").eval(env) match
      case Right(y: _MatrixValue) =>
        Vector(0.0, 0.5, 1.4, 2.62).zipWithIndex.foreach((e, k) => assert(close(y(0, k), e), s"y[$k]"))
      case other => fail(s"unexpected: $other")
  }

  it should "stay as written for a symbolic system, and say why" in
  {
    val e = parse("dlsim(stateSpace([[a]], [[1]], [[1]], 0), [[1, 1]], 0)")
    assert(e.eval(env) == Left(e))
    e match
      case n: _Dlsim => assert(n.detailed(env).left.exists(_.isInstanceOf[SimFailure.NotNumeric]))
      case other     => fail(s"not a dlsim node: $other")
  }

  "the dlsim production" should "round-trip, reserve dlsim and leave states free" in
  {
    for src <- Seq("dlsim(sys, u, x0)", "dlsim(sys, u, x0, states)") do
      val e = parse(src)
      assert(parse(e.toString) == e, src)
    assert(Parser.ReservedWords.contains("dlsim"))
    assert(!Parser.ReservedWords.contains("states"))
    assert(parse("states") == _Variable("states"))
  }
