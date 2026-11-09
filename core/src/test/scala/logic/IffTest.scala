package it.grypho.scala.leonardo
package logic

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0322 (LT L_0079) -- the biconditional `a iff b`.  Before, `p iff q` parsed as the product
 *  `(p * iff) * q`. */
class IffTest extends AnyFlatSpec:

  private val (p, q, a, b, c, d) = (_Variable("p"), _Variable("q"), _Variable("a"), _Variable("b"), _Variable("c"), _Variable("d"))
  private val semantics = Seq(LogicSemantics.MinMax, LogicSemantics.Product, LogicSemantics.Lukasiewicz)
  private val grid      = Seq(0.0, 0.2, 0.5, 0.8, 1.0)

  private def parse(s: String): _Expression = Parser.parse(s) match
    case Parser.Success(e, _) => e
    case other                => fail(s"parse failed for \"$s\": $other")

  private def under(sem: LogicSemantics): Environment =
    new Environment(Environment.DefaultPrecision, Map(), false, sem)
      .withBinding("p", _Bool(true)).withBinding("n", _Bool(false))

  "iff" should "bind looser than implies, and not chain" in {
    assert(parse("p iff q") == Iff(p, q))
    assert(parse("a implies b iff c") == Iff(Implies(a, b), c))
    assert(parse("a iff b implies c") == Iff(a, Implies(b, c)))
    assert(parse("a or b iff c and d") == Iff(Or(a, b), And(c, d)))
    assert(parse("(a iff b) iff c") == Iff(Iff(a, b), c))
    // associative classically, not in a many-valued logic: a chain is refused rather than guessed
    assert(!Parser.parse("a iff b iff c").successful)
  }

  it should "print as the grammar reads it back" in {
    val e = Iff(Implies(a, b), Or(c, d))
    assert(e.toString == "((a implies b) iff (c or d))")
    assert(parse(e.toString) == e)
  }

  it should "be a reserved word, so a name cannot shadow it" in {
    assert(Parser.ReservedWords.contains("iff") && Parser.ReservedWords.contains("equivalent"))
  }

  "the kernel" should "be the conjunction of the two implications, under every semantics" in {
    for sem <- semantics; x <- grid; y <- grid do
      val expected = kleeneAnd(sem)(kleeneImplies(sem)(x, y), kleeneImplies(sem)(y, x))
      assert(math.abs(kleeneIff(sem)(x, y) - expected) < 1e-15, s"$sem $x $y")
  }

  it should "be the negation of xor, by duality" in {
    for sem <- semantics; x <- grid; y <- grid do
      assert(math.abs(kleeneIff(sem)(x, y) - (1.0 - kleeneXor(sem)(x, y))) < 1e-12, s"$sem $x $y")
  }

  "eval" should "answer the classical table, and keep a graded degree" in {
    val env = new Environment()
    def value(s: String) = parse(s).eval(env)
    assert(value("true iff true") == Right(_Bool(true)) && value("false iff false") == Right(_Bool(true)))
    assert(value("true iff false") == Right(_Bool(false)) && value("false iff true") == Right(_Bool(false)))
    assert(value("unknown iff unknown") == Right(_Truth.Unknown) && value("true iff unknown") == Right(_Truth.Unknown))
    assert(value("false iff unknown") == Right(_Truth.Unknown))
    // min(max(1 - 0.8, 0.3), max(1 - 0.3, 0.8)) = min(0.3, 0.8)
    assert(value("truth(0.8) iff truth(0.3)") == Right(_Truth.of(0.3)))
  }

  it should "fold a known operand on either side, under every semantics" in {
    for sem <- semantics do
      val env = under(sem)
      assert(parse("p iff q").eval(env) == Left(q), sem.toString)
      assert(parse("q iff p").eval(env) == Left(q), sem.toString)
      assert(parse("n iff q").eval(env) == Left(Not(q)), sem.toString)
      assert(parse("q iff n").eval(env) == Left(Not(q)), sem.toString)
      assert(parse("p iff n").eval(env) == Right(_Bool(false)), sem.toString)
      assert(parse("n iff n").eval(env) == Right(_Bool(true)), sem.toString)
      assert(parse("p iff unknown").eval(env) == Right(_Truth.Unknown), sem.toString)
  }

  it should "leave a number beside a truth value as written" in {
    assert(parse("3 iff p").eval(under(LogicSemantics.MinMax)) == Left(Iff(_Number(3.0), _Bool(true))))
  }

  "simplifyLogic" should "fold iff and read its two special cases" in {
    def is(src: String, expected: String): Unit =
      assert(simplifyLogicFully(parse(src)) == parse(expected), s"$src -> ${simplifyLogicFully(parse(src))}")
    is("p iff p", "true")
    is("p iff not p", "false")
    is("not p iff p", "false")
    is("not p iff not q", "p iff q")
    is("true iff q", "q")
    is("q iff false", "not q")
    is("not (p iff q)", "p xor q")
    is("not (p xor q)", "p iff q")
  }

  it should "not apply the complement or idempotence laws to a graded operand" in {
    val g = And(_Truth.of(0.9), q)
    assert(simplifyLogicFully(Iff(g, g)) == Iff(g, g))
    assert(simplifyLogicFully(Iff(g, Not(g))) == Iff(g, Not(g)))
  }

  "the normal forms" should "desugar iff" in {
    assert(toCNF(Iff(p, q)) == parse("(not p or q) and (p or not q)"))
  }

  it should "keep the value of a formula with iff, on a generated corpus" in {
    val random = new scala.util.Random(322)
    val atoms  = List(a, b, c)
    def tree(depth: Int): _Expression =
      if depth == 0 || random.nextInt(5) == 0 then atoms(random.nextInt(3))
      else random.nextInt(6) match
        case 0 => And(tree(depth - 1), tree(depth - 1))
        case 1 => Or(tree(depth - 1), tree(depth - 1))
        case 2 => Not(tree(depth - 1))
        case 3 => Implies(tree(depth - 1), tree(depth - 1))
        case 4 => Xor(tree(depth - 1), tree(depth - 1))
        case _ => Iff(tree(depth - 1), tree(depth - 1))
    def table(e: _Expression): List[Option[Boolean]] =
      for i <- (0 until 8).toList yield
        val env = atoms.zipWithIndex.foldLeft(new Environment()) { case (acc, (v, j)) =>
          acc.withBinding(v.variable, _Bool(((i >> j) & 1) == 1)) }
        e.eval(env) match
          case Right(_Bool(x)) => Some(x)
          case _               => None
    def mentions(e: _Expression): Boolean = e.isInstanceOf[Iff] || e.isInstanceOf[Implies] || e.isInstanceOf[Xor] || e.children.exists(mentions)
    def size(e: _Expression): Int = 1 + e.children.map(size).sum
    // toCNF/toDNF answer their input past the clause cap, which looks like a normal form that kept its
    // connectives; the cnf/dnf nodes stay unevaluated instead, so a refusal can be told apart and skipped
    def normal(kind: NormalKind, e: _Expression): Option[_Expression] =
      _NormalForm(kind, e).eval(new Environment()) match
        case Left(_: _NormalForm) => None
        case Left(x)              => Some(x)
        case Right(v)             => Some(v)
    var checked = 0
    for _ <- 1 to 400 do
      val e = tree(4)
      assert(!table(e).contains(None), s"$e did not reduce")
      val simp = simplifyLogicFully(e)
      assert(table(simp) == table(e) && size(simp) <= size(e), s"$e simplified to $simp")
      for kind <- List(NormalKind.CNF, NormalKind.DNF); nf <- normal(kind, e) do
        checked += 1
        assert(table(nf) == table(e), s"$kind of $e is $nf")
        assert(!mentions(nf), s"$kind of $e still holds a desugarable connective: $nf")
    assert(checked > 500, checked)
  }
