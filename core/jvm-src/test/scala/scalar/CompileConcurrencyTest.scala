package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

/** F_0058: one compiled closure shared by several threads answers as it does alone.  JVM-only,
 *  since Node has no threads; the cross-platform half of the claim (no state survives a call)
 *  is `CompileTest`'s interleaving case. */
class CompileConcurrencyTest extends AnyFlatSpec:

  "a compiled closure" should "give every thread the sequential answers" in {
    val vars = Seq(_Variable("x"), _Variable("y"))
    val f = compile(Parser.parse("sin(x)*exp(-y) + x^2/(1 + y^2)").get, vars, new Environment())
      .getOrElse(fail("did not compile"))
    val points   = Vector.tabulate(20000)(k => Array(k * 1e-3, 2 - k * 7e-5))
    val expected = points.map(f)
    val runs = Vector.fill(8)(Future(points.map(p => f(p.clone))))
    Await.result(Future.sequence(runs), 60.seconds).foreach(r => assert(r == expected))
  }
