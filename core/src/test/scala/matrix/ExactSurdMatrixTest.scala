package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0231 Decision B -- the exact matrix kernels eliminate over `Q(√d)` when every surd cell shares
 *  one radical, with the same code as over `Q`; cells of two radicals are read at the working
 *  precision, as every kernel read them before. */
class ExactSurdMatrixTest extends AnyFlatSpec:

  private val env = new Environment()

  private def eval(s: String): _Expression =
    Parser.parse(s, Some(30)) match
      case Parser.Success(e, _) => e.eval(env).fold(identity, identity)
      case other                => fail(s"parse failed for \"$s\": $other")

  private def truth(s: String): Boolean = eval(s) match
    case _Bool(b) => b
    case other    => fail(s"'$s' gave no truth value: $other")

  private def sqrt(n: Int): _Value = _Surd.sqrtOf(_Rational(n)).getOrElse(fail(s"√$n"))

  "det" should "be exact over one radical, written in any form" in {
    assert(eval("det([[√2, 1], [1, √2]])") == _Rational(1))
    assert(eval("det([[1, √2], [√8, 3]])") == _Rational(-1))   // √8 = 2√2: one radical
    assert(eval("det([[√3, 0], [0, 2]])") == _Surd.multiply(sqrt(3), _Rational(2)).getOrElse(fail("2√3")))
  }

  it should "read two radicals at the working precision, never as a Double" in {
    eval("det([[√2, √3], [1, 1]])") match
      case r: _Rational => assert(math.abs(r.toDouble - (math.sqrt(2) - math.sqrt(3))) < 1e-15)
      case other        => fail(s"not the working-precision rational: $other")
  }

  "inv" should "be exact over one radical" in {
    assert(eval("inv([[√2, 1], [1, √2]])") ==
      _Matrix(2, 2, Vector(sqrt(2), _Rational(-1), _Rational(-1), sqrt(2))))
    assert(truth("inv([[1, √3], [√3, 1]]) * [[1, √3], [√3, 1]] == eye(2)"))
  }

  "A^n" should "be exact over one radical" in {
    assert(eval("[[1, √2], [0, 1]]^3") ==
      _Matrix(2, 2, Vector(_Rational(1), _Surd.multiply(sqrt(2), _Rational(3)).getOrElse(fail("3√2")), _Rational(0), _Rational(1))))
    assert(truth("[[√2, 1], [1, √2]]^(-1) == inv([[√2, 1], [1, √2]])"))
  }

  "lu" should "factor over one radical, so P·A = L·U exactly" in {
    val a = "[[1, √2], [√2, 4]]"
    // The factors are multiplied as matrix products directly: the bare parser cannot know that
    // two `at(...)` results are matrices (the REPL re-dispatches after substitution, F_0089).
    (eval(s"lu($a)"), eval(a)) match
      case (_Matrix(1, 3, Vector(l: _Matrix, u: _Matrix, p: _Matrix)), am: _Matrix) =>
        assert((l.elems ++ u.elems ++ p.elems).forall(_Surd.isExact), l)
        assert(MatProduct(l, u).eval(env).fold(identity, identity) == MatProduct(p, am).eval(env).fold(identity, identity))
      case other => fail(s"$other")
  }
