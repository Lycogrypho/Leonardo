package it.grypho.scala.leonardo
package logic

import core.*
import scalar.{Sum, simplifyFully}
import equation.{_Equation}
import org.scalatest.flatspec.AnyFlatSpec


class LogicSimplifyTest extends AnyFlatSpec:

  val a = _Variable("a")
  val b = _Variable("b")
  val c = _Variable("c")
  val T = _Bool(true)
  val F = _Bool(false)

  /** All free variables of `e`, sorted, as `_Variable`s. */
  def varsOf(e: _Expression): List[_Variable] = e.freeVars.toList.sorted.map(_Variable.apply)

  /** Asserts `e1` and `e2` evaluate identically under every boolean assignment. */
  def assertEquivalent(e1: _Expression, e2: _Expression): Unit =
    val vars = (e1.freeVars ++ e2.freeVars).toList.sorted.map(_Variable.apply)
    val t1 = truthTable(e1, vars)
    val t2 = truthTable(e2, vars)
    assert(t1 == t2, s"not equivalent:\n  $e1\n  $e2")

  /** True when `e` is a literal: an atom or a negated atom. */
  def isLiteral(e: _Expression): Boolean = e match
    case Not(x)          => !x.isInstanceOf[_Connective]
    case _: _Connective  => false
    case _               => true

  /** True when `e` is in CNF: a conjunction of disjunctions of literals. */
  def isCNF(e: _Expression): Boolean = e match
    case And(x, y) => isCNF(x) && isCNF(y)
    case other     => isDisjunctionOfLiterals(other)
  def isDisjunctionOfLiterals(e: _Expression): Boolean = e match
    case Or(x, y) => isDisjunctionOfLiterals(x) && isDisjunctionOfLiterals(y)
    case other    => isLiteral(other)

  /** True when `e` is in DNF: a disjunction of conjunctions of literals. */
  def isDNF(e: _Expression): Boolean = e match
    case Or(x, y) => isDNF(x) && isDNF(y)
    case other    => isConjunctionOfLiterals(other)
  def isConjunctionOfLiterals(e: _Expression): Boolean = e match
    case And(x, y) => isConjunctionOfLiterals(x) && isConjunctionOfLiterals(y)
    case other     => isLiteral(other)

  // --- simplifyLogic rules ---

  "constant folding" should "apply the identity and absorbing elements" in
  {
    assert(simplifyLogicFully(And(a, T)) == a)
    assert(simplifyLogicFully(And(T, a)) == a)
    assert(simplifyLogicFully(And(a, F)) == F)
    assert(simplifyLogicFully(Or(a, F)) == a)
    assert(simplifyLogicFully(Or(F, a)) == a)
    assert(simplifyLogicFully(Or(a, T)) == T)
  }

  "double negation" should "cancel" in
  {
    assert(simplifyLogicFully(Not(Not(a))) == a)
    assert(simplifyLogicFully(Not(Not(Not(a)))) == Not(a))
  }

  "idempotence" should "fold a and a to a, a or a to a" in
  {
    assert(simplifyLogicFully(And(a, a)) == a)
    assert(simplifyLogicFully(Or(a, a)) == a)
  }

  "complement" should "fold a and not a to false, a or not a to true" in
  {
    assert(simplifyLogicFully(And(a, Not(a))) == F)
    assert(simplifyLogicFully(And(Not(a), a)) == F)
    assert(simplifyLogicFully(Or(a, Not(a))) == T)
    assert(simplifyLogicFully(Or(Not(a), a)) == T)
  }

  "absorption" should "fold a or (a and b) to a, a and (a or b) to a" in
  {
    assert(simplifyLogicFully(Or(a, And(a, b))) == a)
    assert(simplifyLogicFully(Or(And(b, a), a)) == a)
    assert(simplifyLogicFully(And(a, Or(a, b))) == a)
    assert(simplifyLogicFully(And(Or(b, a), a)) == a)
  }

  "implies folding" should "apply the constant cases" in
  {
    assert(simplifyLogicFully(Implies(F, a)) == T)
    assert(simplifyLogicFully(Implies(T, a)) == a)
    assert(simplifyLogicFully(Implies(a, T)) == T)
    assert(simplifyLogicFully(Implies(a, F)) == Not(a))
    assert(simplifyLogicFully(Implies(a, a)) == T)
  }

  "xor folding" should "apply the constant cases" in
  {
    assert(simplifyLogicFully(Xor(a, F)) == a)
    assert(simplifyLogicFully(Xor(F, a)) == a)
    assert(simplifyLogicFully(Xor(a, T)) == Not(a))
    assert(simplifyLogicFully(Xor(a, a)) == F)
    assert(simplifyLogicFully(Xor(T, T)) == F)
  }

  "nested constants" should "fold through the fixpoint" in
  {
    // not (a and true) or false  ->  not a
    assert(simplifyLogicFully(Or(Not(And(a, T)), F)) == Not(a))
  }

  "a scalar body inside a connective" should "be simplified by the injected leaf pass" in
  {
    // not (x + 0 = x): the equation is _ElementWise, so scalar simplify folds x + 0
    val e = Not(_Equation(Sum(_Variable("x"), _Number(0)), _Variable("x")))
    assert(simplifyLogic(e, simplifyFully) == Not(_Equation(_Variable("x"), _Variable("x"))))
    // without the leaf pass the scalar body is untouched
    assert(simplifyLogic(e) == e)
  }

  "De Morgan and desugaring" should "NOT be applied by simplifyLogic" in
  {
    assert(simplifyLogicFully(Not(And(a, b))) == Not(And(a, b)))
    assert(simplifyLogicFully(Implies(a, b)) == Implies(a, b))
    assert(simplifyLogicFully(Xor(a, b)) == Xor(a, b))
  }

  // --- normal forms ---

  "toCNF of not (a and b)" should "be the De Morgan disjunction" in
  {
    assert(toCNF(Not(And(a, b))) == Or(Not(a), Not(b)))
  }

  "toDNF of not (a or b)" should "be the De Morgan conjunction" in
  {
    assert(toDNF(Not(Or(a, b))) == And(Not(a), Not(b)))
  }

  "toCNF of a implies b" should "desugar to (not a) or b" in
  {
    assert(toCNF(Implies(a, b)) == Or(Not(a), b))
  }

  "toCNF of a distribution case" should "distribute or over and" in
  {
    // a or (b and c)  ->  (a or b) and (a or c)
    val cnf = toCNF(Or(a, And(b, c)))
    assert(isCNF(cnf), s"not CNF: $cnf")
    assertEquivalent(cnf, Or(a, And(b, c)))
  }

  "toDNF of a distribution case" should "distribute and over or" in
  {
    // a and (b or c)  ->  (a and b) or (a and c)
    val dnf = toDNF(And(a, Or(b, c)))
    assert(isDNF(dnf), s"not DNF: $dnf")
    assertEquivalent(dnf, And(a, Or(b, c)))
  }

  "toCNF and toDNF of an xor chain" should "stay equivalent and in normal form" in
  {
    val e = Xor(a, Xor(b, c))
    val cnf = toCNF(e)
    val dnf = toDNF(e)
    assert(isCNF(cnf), s"not CNF: $cnf")
    assert(isDNF(dnf), s"not DNF: $dnf")
    assertEquivalent(cnf, e)
    assertEquivalent(dnf, e)
  }

  "toCNF of a tautology" should "fold to true" in
  {
    assert(toCNF(Or(a, Not(a))) == T)
    // (a or not a) and (b or not b): every clause trivially true
    assert(toCNF(And(Or(a, Not(a)), Or(b, Not(b)))) == T)
  }

  "toDNF of a contradiction" should "fold to false" in
  {
    assert(toDNF(And(a, Not(a))) == F)
  }

  "toCNF of constants and atoms" should "pass through" in
  {
    assert(toCNF(T) == T)
    assert(toCNF(a) == a)
    assert(toCNF(Not(a)) == Not(a))
  }

  "an exponential blow-up" should "return the input unchanged" in
  {
    // a chain of n xors distributes into 2^(n-1) clauses; 12 -> 2048 > MaxNormalFormClauses
    val vars = (1 to 12).map(i => _Variable(s"v$i")).toList
    val chain = vars.map(v => v: _Expression).reduceLeft(Xor.apply)
    assert(toCNF(chain) == chain)
  }

  // --- truth tables ---

  "truthTable of a and b" should "enumerate the four rows in order" in
  {
    val rows = truthTable(And(a, b), List(a, b))
    assert(rows.size == 4)
    assert(rows.map(_._2) == Vector(Some(false), Some(false), Some(false), Some(true)))
    assert(rows.head._1 == Map("a" -> false, "b" -> false))
    assert(rows.last._1 == Map("a" -> true, "b" -> true))
  }

  "truthTable rows that cannot reduce" should "yield None, short-circuit rows Some" in
  {
    // x stays free: a = false short-circuits to false, a = true stays symbolic
    val e = And(a, _Equation(_Variable("x"), _Number(1)))
    val rows = truthTable(e, List(a))
    assert(rows.map(_._2) == Vector(Some(false), None))
  }

  "truthTable with no variables" should "yield the single empty-assignment row" in
  {
    val rows = truthTable(And(T, F), Nil)
    assert(rows == Vector((Map.empty[String, Boolean], Some(false))))
  }

  "truthTable beyond the variable cap" should "be rejected with an empty result" in
  {
    val vars = (1 to MaxTruthTableVars + 1).map(i => _Variable(s"v$i")).toList
    val e = vars.map(v => v: _Expression).reduceLeft(And.apply)
    assert(truthTable(e, vars).isEmpty)
  }
