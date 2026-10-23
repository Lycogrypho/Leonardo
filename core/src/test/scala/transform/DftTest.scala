package it.grypho.scala.leonardo
package transform

import core.*
import matrix._Matrix
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** `dft`, `idft` and `conv` (F_0137): discrete-time Fourier analysis by the direct sum. */
class DftTest extends AnyFlatSpec:

  private val env = new Environment()
  private def ev(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)

  /** The cells of an evaluated vector as `(re, im)`. */
  private def cells(r: Either[_Expression, _Value]): Vector[(Double, Double)] = r match
    case Right(m: _MatrixValue) => m.toVector.map(d => (d, 0.0))
    case Left(m: _Matrix)       => m.elems.collect { case v: _Value => _Complex.parts(v) }.flatten
    case other                  => fail(s"not a vector: $other")

  private def near(a: Vector[(Double, Double)], b: Vector[(Double, Double)], tol: Double = 1e-12): Boolean =
    a.size == b.size && a.zip(b).forall((p, q) => math.hypot(p._1 - q._1, p._2 - q._2) <= tol)

  "dft" should "put a pure tone in a single bin, N/2 high, and its mirror" in {
    // cos(2*pi*n/8) for n = 0..7: bins 1 and 7, each 4.
    val x = (0 until 8).map(n => math.cos(2 * math.Pi * n / 8)).mkString("[[", ", ", "]]")
    val bins = cells(ev(s"dft($x)"))
    assert(bins.size == 8)
    assert(near(bins, Vector.tabulate(8)(k => if k == 1 || k == 7 then (4.0, 0.0) else (0.0, 0.0))))
  }

  it should "be undone by idft" in {
    val x = "[[1, -2, 3.5, 0, 4, -1]]"
    assert(near(cells(ev(s"idft(dft($x))")), cells(ev(x))))
  }

  it should "keep Parseval's identity: sum |x|^2 = sum |X|^2 / N" in {
    val xs = Vector(1.0, -2.0, 3.5, 0.0, 4.0, -1.0)
    val bins = cells(ev(xs.mkString("dft([[", ", ", "]])")))
    assert(math.abs(xs.map(x => x * x).sum - bins.map((a, b) => a * a + b * b).sum / xs.size) < 1e-10)
  }

  it should "keep the shape of its operand and accept complex values" in {
    ev("dft([[1], [i], [-1], [-i]])") match
      case Left(m: _Matrix) => assert(m.rows == 4 && m.cols == 1)
      case other            => fail(s"got $other")
    // x = [1, i, -1, -i] = e^(i*pi*n/2): one bin, k = 1, of height 4.
    assert(near(cells(ev("dft([[1], [i], [-1], [-i]])")), Vector((0.0, 0.0), (4.0, 0.0), (0.0, 0.0), (0.0, 0.0))))
  }

  it should "stay unevaluated past the environment's length budget" in {
    val tight = env.withLimit(Limit.MaxDftLength, 4)
    assert(Parser.parse("dft([[1, 2, 3, 4, 5]])").get.eval(tight).isLeft)
  }

  it should "transform a sequence at the default length cap promptly, on every platform, under load" in {
    val n = Limit.MaxDftLength.default.toInt
    val start = System.nanoTime()
    val out = dft(Vector.tabulate(n)(t => (math.sin(t * 0.01), 0.0)), inverse = false)
    val seconds = (System.nanoTime() - start) / 1e9
    assert(out.size == n)
    // F_0179: 1.5-1.7 s on Node alone and 0.6-1.3 s on the JVM, but 2.9-7.6 s on Node under the full
    // `sbt test`, which runs the JVM and Node suites at once; the first bound, 5 s, set from unloaded
    // timings, failed there twice. 15 s keeps 2x over the worst loaded run (the GroebnerTest rule).
    assert(seconds < 15.0, s"took $seconds s")
  }

  "conv" should "multiply polynomials: (1 + 2x)(3 + x + x^2)" in {
    assert(ev("conv([[1, 2]], [[3, 1, 1]])") == ev("[[3, 7, 3, 2]]"))
  }

  it should "stay exact on exact input" in {
    val exact = Parser.parse("conv([[1/2, 1/3]], [[3, 6]])", Some(30)).get.eval(env)
    assert(exact == Parser.parse("[[3/2, 4, 2]]", Some(30)).get.eval(env))
  }

  it should "agree with the DFT: dft of a convolution is the product of the padded dfts" in {
    val (x, h) = ("[[1, 2, 0, 0]]", "[[3, 1, 1, 0]]")
    val product = cells(ev(s"dft($x)")).zip(cells(ev(s"dft($h)"))).map((a, b) => (a._1 * b._1 - a._2 * b._2, a._1 * b._2 + a._2 * b._1))
    // The linear convolution has length 4 + 4 - 1 = 7; its first four terms carry the circular one,
    // since the tail (zeros here) wraps onto nothing.
    val linear = cells(ev(s"conv($x, $h)"))
    val circular = Vector.tabulate(4)(k => (linear(k)._1 + linear.lift(k + 4).fold(0.0)(_._1), 0.0))
    assert(near(cells(ev(circular.map(_._1).mkString("dft([[", ", ", "]])"))), product, 1e-10))
  }

  it should "bound its products, not only its output (F_0144)" in {
    // 4 x 4 = 16 products against a budget of 10: the output, 7 cells, was all that was checked.
    val tight = env.withLimit(Limit.MaxConvWork, 10)
    assert(Parser.parse("conv([[1, 2, 3, 4]], [[1, 2, 3, 4]])").get.eval(tight).isLeft)
    assert(Parser.parse("conv([[1, 2, 3]], [[1, 2, 3]])").get.eval(tight).isRight)
    Parser.parse("conv([[1, 2, 3, 4]], [[1, 2, 3, 4]])").get match
      case c: _Conv => assert(c.declineReason(tight).exists(_.contains("MaxConvWork")), c.declineReason(tight))
      case other    => fail(s"not a conv: $other")
  }

  it should "hold an exact convolution to its own, smaller budget (F_0144)" in {
    val tight = env.withLimit(Limit.MaxConvExactWork, 4)
    assert(Parser.parse("conv([[1/2, 1/3, 1/5]], [[3, 6, 7]])", Some(30)).get.eval(tight).isLeft)
    assert(Parser.parse("conv([[0.5, 0.25, 0.2]], [[3, 6, 7]])").get.eval(tight).isRight)
  }

  it should "convolve at the default work budget within a few seconds, on every platform (F_0144)" in {
    val n = math.sqrt(Limit.MaxConvWork.default.toDouble).toInt
    val xs = Vector.tabulate(n)(i => _Number(math.sin(i * 0.1)): _Value)
    val start = System.nanoTime()
    val out = convolve(xs, xs, env)
    val seconds = (System.nanoTime() - start) / 1e9
    assert(out.size == 2 * n - 1)
    // F_0179: 0.2 s on Node alone, at most 0.4 s under the full run; the 5 s bound has the margin.
    assert(seconds < 5.0, s"took $seconds s")
  }

  "dft" should "say which budget refused it (F_0144)" in {
    val tight = env.withLimit(Limit.MaxDftLength, 4)
    Parser.parse("dft([[1, 2, 3, 4, 5]])").get match
      case d: _Dft => assert(d.declineReason(tight).exists(_.contains("MaxDftLength")), d.declineReason(tight))
      case other   => fail(s"not a dft: $other")
  }

  "the new forms" should "round-trip and reserve their words" in {
    for s <- List("dft(x)", "idft(X)", "conv(x, h)") do
      val e = Parser.parse(s).get
      assert(Parser.parse(e.toString).get == e, s)
    List("dft", "idft", "conv").foreach(w => assert(Parser.ReservedWords.contains(w), w))
  }
