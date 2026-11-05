package it.grypho.scala.leonardo
package core

import org.scalatest.flatspec.AnyFlatSpec
import parser.Parser


/** F_0273 (LT L_0059, L_0060): under the session's exact mode the constant-inventing passes build
 *  exact constants even where the input carries no exact value of its own. */
class ExactModeReachTest extends AnyFlatSpec:

  private val exactEnv   = new Environment().withExactMode(true)
  private val defaultEnv = new Environment()

  private def shown(src: String, env: Environment, exactParse: Boolean): String =
    val p = Parser.parse(src, if exactParse then Some(30) else None)
    assert(p.successful, s"parse failed: $src")
    p.get.eval(env).fold(_.toString, _.toString)

  "a letters-only input under exact mode" should "get exact constants (F_0273)" in {
    for src <- Seq("derive(atan(x), x)", "derive(x, x)", "integral(y, y)", "integral(x*ln(x), x)",
                   "div([[x], [y], [z]], x, y, z)", "sum(x^k, k, 1, 5)", "derive(sin(x)^2, x)") do
      val out = shown(src, exactEnv, exactParse = true)
      assert(!out.matches("(?s).*\\d\\.\\d.*"), s"$src gave decimals: $out")
    assert(shown("derive(x, x)", exactEnv, exactParse = true) == "1")
    assert(shown("div([[x], [y], [z]], x, y, z)", exactEnv, exactParse = true) == "3")
    assert(shown("integral(integral(1, y, 0, x), x, 0, 1)", exactEnv, exactParse = true) == "1/2")
  }

  "the default mode" should "be unchanged" in {
    assert(shown("derive(atan(x), x)", defaultEnv, exactParse = false) == "(1.0 / (1.0 + (x ^ 2.0)))")
    assert(shown("derive(x, x)", defaultEnv, exactParse = false) == "1.0")
  }

  "exactMode" should "be threaded through bindings and limits" in {
    assert(exactEnv.withBinding("a", _Number(1)).exactMode)
    assert(exactEnv.withLimit(Limit.MaxTaylorOrder, 5).exactMode)
    assert(!defaultEnv.exactMode)
  }
