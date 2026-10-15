package it.grypho.scala.leonardo
package optimize

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0124: the work and size budgets are `Limit`s carried by `Environment`, so a library caller can
 *  raise one where the browser's default refuses, or lower one. The default is today's value, so
 *  every other suite is the evidence that the default path is unchanged; these cases show that
 *  each budget is actually read from the environment. */
class LimitsTest extends AnyFlatSpec:

  private val env = new Environment()
  private def parse(s: String): _Expression = Parser.parse(s).get
  private def vars(ns: String*): Vector[_Variable] = ns.map(_Variable(_)).toVector

  "Limits" should "default to the published constants, and refuse a budget below one" in {
    assert(Limits.Default(Limit.MaxQpVariables) == MaxQpVariables)
    assert(Limits.Default(Limit.MaxGroebnerBasis) == MaxGroebnerBasis)
    assert(Limits.Default(Limit.MaxLsqWork) == MaxLsqWork)
    val raised = Limits.Default.withLimit(Limit.MaxLpVariables, 41)
    assert(raised(Limit.MaxLpVariables) == 41 && raised.overridden == Map(Limit.MaxLpVariables -> 41L))
    assert(raised.withLimit(Limit.MaxLpVariables, Limit.MaxLpVariables.default) == Limits.Default)
    assertThrows[IllegalArgumentException](Limits.Default.withLimit(Limit.MaxQpIterations, 0))
    assert(Limits.Default.withLimit(Limit.MaxLsqWork, Long.MaxValue).count(Limit.MaxLsqWork) == Int.MaxValue)
  }

  "Environment" should "carry its limits through a binding, and keep the published constructor" in {
    val e = env.withLimit(Limit.MaxQpVariables, 7)
    assert(e.withBinding("x", _Number(1)).limits(Limit.MaxQpVariables) == 7)
    val old = new Environment(5, Map.empty[String, _Value], false, LogicSemantics.MinMax, 30)
    assert(old.limits == Limits.Default)
  }

  "qpSolve" should "read its variable, constraint and iteration caps from the environment" in {
    // min ½(x² + y²) − 3x − 3y on x ≤ 1, y ≤ 1: both constraints are active at the optimum
    // (1, 1), so Goldfarb–Idnani adds two — two iterations.
    val h = _MatrixValue(2, 2, Array(1.0, 0.0, 0.0, 1.0))
    def run(at: Environment) =
      qpSolve(h, Vector(-3.0, -3.0), Some((_MatrixValue(2, 2, Array(1.0, 0.0, 0.0, 1.0)), Vector(1.0, 1.0))), None,
              Vector.fill(2)(Double.NegativeInfinity), Vector.fill(2)(Double.PositiveInfinity), Set.empty, at)
    assert(run(env).exists(_.z == Vector(1.0, 1.0)))
    assert(run(env.withLimit(Limit.MaxQpVariables, 1)).left.exists(_.isInstanceOf[QpFailure.InvalidInput]))
    assert(run(env.withLimit(Limit.MaxQpConstraints, 1)).left.exists(_.isInstanceOf[QpFailure.InvalidInput]))
    assert(run(env.withLimit(Limit.MaxQpIterations, 1)) == Left(QpFailure.MaxIterations(1)))
  }

  "lpSolve" should "answer a problem past the default variable cap once the cap is raised" in {
    // min Σxᵢ on x ≥ 1, 41 variables: one past MaxLpVariables.
    val n  = MaxLpVariables + 1
    val one: _Value = _Number(1.0)
    def run(limits: Limits) = lpSolve(Vector.fill(n)(one), None, None, Vector.fill(n)(one),
                                      Vector.fill(n)(_Number(Double.PositiveInfinity): _Value), limits)
    assert(run(Limits.Default).left.exists(_.isInstanceOf[LpFailure.InvalidInput]))
    run(Limits.Default.withLimit(Limit.MaxLpVariables, n)) match
      case Right(r) => assert(r.objective == _Number(n.toDouble))
      case Left(w)  => fail(s"declined with the cap raised: $w")
    // The prompt's node reads the same limits from its environment.
    val node = parse(s"lp(ones($n, 1), 0, 0, 1, inf)")
    assert(node.eval(env).isLeft)
    assert(node.eval(env.withLimit(Limit.MaxLpVariables, n)).isRight)
  }

  "minimize" should "stop at the iteration cap the environment sets" in {
    val rosenbrock = parse("(1 - x)^2 + 100*(y - x^2)^2")
    def run(at: Environment) = minimize(rosenbrock, vars("x", "y"), Vector(-1.2, 1.0), None, MinimizeMethod.BFGS, at)
    assert(run(env).isRight)
    assert(run(env.withLimit(Limit.MaxMinimizeIterations, 3)).left.exists {
      case MinimizeFailure.MaxIterations(3, _) => true
      case _                                   => false
    })
  }

  "nlpSolve" should "read its variable, iteration and round caps from the environment" in {
    val (f, g) = (parse("x + y"), parse("x^2 + y^2 - 2"))
    def run(m: NlpMethod, at: Environment) = nlpSolve(f, vars("x", "y"), Vector(1.0, 0.0), Vector(g), Vector.empty, None, m, at)
    assert(run(NlpMethod.SQP, env).isRight && run(NlpMethod.AugLag, env).isRight)
    assert(run(NlpMethod.SQP, env.withLimit(Limit.MaxNlpVariables, 1)).left.exists(_.isInstanceOf[NlpFailure.InvalidInput]))
    assert(run(NlpMethod.SQP, env.withLimit(Limit.MaxNlpIterations, 1)).left.exists(_.isInstanceOf[NlpFailure.MaxIterations]))
    assert(run(NlpMethod.AugLag, env.withLimit(Limit.MaxAugLagRounds, 1)).left.exists(_.isInstanceOf[NlpFailure.MaxIterations]))
  }

  "lsqfit" should "read its iteration and work caps from the environment" in {
    val r = parse("[[10*(b - a^2)], [1 - a]]")
    def run(at: Environment) = lsqfit(r, vars("a", "b"), Vector(-1.2, 1.0), None, at)
    assert(run(env).isRight)
    assert(run(env.withLimit(Limit.MaxLsqIterations, 2)).left.exists(_.isInstanceOf[LsqFailure.MaxIterations]))
    assert(run(env.withLimit(Limit.MaxLsqWork, 7)).left.exists(_.isInstanceOf[LsqFailure.TooLarge]))
  }

  "convexity, kktPoints, eliminate and groebnerBasis" should "read their caps from the environment" in {
    assert(convexity(parse("x^2 + y^2"), vars("x", "y"), env).contains(true))
    assert(convexity(parse("x^2 + y^2"), vars("x", "y"), env.withLimit(Limit.MaxConvexityVariables, 1)).isEmpty)

    val (f, g) = (parse("(x - 2)^2 + (y - 2)^2"), parse("[[x - 1], [y - 1]]"))
    assert(kktPoints(f, g, _Number(0), vars("x", "y"), env).isDefined)
    assert(kktPoints(f, g, _Number(0), vars("x", "y"), env.withLimit(Limit.MaxKKTInequalities, 1)).isEmpty)

    val coupled = List(parse("x^2 + y^2 - 5"), parse("x^2 - y^2 - 3"))
    assert(eliminate(coupled, vars("x", "y").toList, env).exists(_.size == 4))
    assert(eliminate(coupled, vars("x", "y").toList, env.withLimit(Limit.MaxEliminationSteps, 1)).isEmpty)

    val three = List(parse("x^2 + y^2 + z^2 - 3"), parse("x^2 - y^2"), parse("y^2 - z^2"))
    assert(groebnerBasis(three, vars("x", "y", "z").toList, env).isDefined)
    assert(groebnerBasis(three, vars("x", "y", "z").toList, env.withLimit(Limit.MaxGroebnerDegree, 1)).isEmpty)
    assert(groebnerBasis(three, vars("x", "y", "z").toList, env.withLimit(Limit.MaxGroebnerBasis, 2)).isEmpty)
  }
