package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0230: `eigen` and `eig` list eigenvalues by real part ascending, then imaginary part — the
 *  order `tfpoles` answers and `jordan`'s exact path uses — not the QR iteration's order. */
class EigenOrderTest extends AnyFlatSpec:

  private val env = Environment()
  private def run(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)

  private def reals(e: Either[_Expression, _Value]): Vector[Double] = e match
    case Left(m: _Matrix) => m.elems.map { case _Number(d) => d; case o => fail(s"not real: $o") }
    case other            => fail(s"not a row: $other")

  "eigen of a diagonal matrix" should "list its eigenvalues ascending" in {
    assert(reals(run("eigen([[4, 0, 0], [0, -2, 0], [0, 0, 3]])")) == Vector(-2.0, 3.0, 4.0))
  }

  "eig" should "order D the same way, with V's columns permuted alongside" in {
    run("eig([[4, 0, 0], [0, -2, 0], [0, 0, 3]])") match
      case Left(_Matrix(1, 2, Vector(v: _Matrix, d: _Matrix))) =>
        assert((0 until 3).map(i => d(i, i)) == Vector(_Number(-2), _Number(3), _Number(4)))
        // The eigenvector of -2 is e2, so V's first column must be (0, ±1, 0).
        assert((0 until 3).map(i => v(i, 0)).collect { case _Number(x) => math.abs(x) } == Vector(0.0, 1.0, 0.0))
      case other => fail(s"unexpected: $other")
  }

  it should "still satisfy A V = V D on a dense non-symmetric matrix" in {
    val a = "[[2, 1, 0], [1, 3, 1], [0, 1, 5]]"
    run(s"eig($a)") match
      case Left(_Matrix(1, 2, Vector(v: _Matrix, d: _Matrix))) =>
        val diag = (0 until 3).map(i => d(i, i)).collect { case _Number(x) => x }
        assert(diag == diag.sorted)
        (MatProduct(Parser.parse(a).get, v).eval(env), MatProduct(v, d).eval(env)) match
          case (Right(av: _MatrixValue), Right(vd: _MatrixValue)) =>
            assert(av.toVector.zip(vd.toVector).forall((p, q) => math.abs(p - q) < 1e-9))
          case other => fail(s"not dense: $other")
      case other => fail(s"unexpected: $other")
  }

  "a complex pair" should "come in the order tfpoles answers" in {
    val eigs  = run("eigen([[1, -2], [2, 1]])")
    val poles = Parser.parse("tfpoles(1/(s^2 - 2*s + 5), s)").get.eval(env)
    def parts(r: Either[_Expression, _Value]): Vector[(Double, Double)] = r match
      case Left(m: _Matrix) => m.elems.collect { case c: _Complex => _Complex.parts(c).getOrElse((0.0, 0.0)) }
      case other            => fail(s"not a row: $other")
    val (e, p) = (parts(eigs), parts(poles))
    assert(e.size == 2 && p.size == 2)
    assert(e.zip(p).forall { case ((a, b), (c, d)) => math.abs(a - c) < 1e-9 && math.abs(b - d) < 1e-9 })
  }

  "jordan's numeric path" should "use the same order as eig" in {
    run("jordan([[4, 0], [0, -2]])") match
      case Left(_Matrix(1, 2, Vector(_, j: _Matrix))) => assert(j(0, 0) == _Number(-2) && j(1, 1) == _Number(4))
      case other                                      => fail(s"unexpected: $other")
  }
