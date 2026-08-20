package it.grypho.scala.leonardo
package logic

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class FuzzyLogicTest extends AnyFlatSpec:

  val a = _Variable("a")
  val x = _Variable("x")
  val T = _Bool(true)
  val F = _Bool(false)
  val U = _Truth.Unknown

  val Tol = 1e-9

  /** An environment under the given semantics. */
  def envOf(s: LogicSemantics): Environment =
    new Environment(Environment.DefaultPrecision, Map.empty, false, s)

  val minmax:  Environment = envOf(LogicSemantics.MinMax)
  val product: Environment = envOf(LogicSemantics.Product)
  val lukas:   Environment = envOf(LogicSemantics.Lukasiewicz)

  def parse(input: String): _Expression =
    val result = Parser.parse(input)
    assert(result.successful, s"parse failed for \"$input\": $result")
    result.get

  /** The degree an expression evaluates to, failing when it is not truth-valued. */
  def degree(e: _Expression, env: Environment = minmax): Double =
    e.eval(env) match
      case Right(v) => asTruth(v, env.symmetricLogic) match
        case Some(d) => d
        case None    => fail(s"not truth-valued: $v")
      case other => fail(s"did not reduce: $other")

  def assertClose(actual: Double, expected: Double, clue: String = ""): Unit =
    assert(math.abs(actual - expected) <= 1e-6, s"$clue expected $expected but got $actual")

  /** A truth value at degree `d`. */
  def t(d: Double): _Expression = _Truth.of(d)

  // --- min-max identities on interior degrees ---

  "min-max on interior degrees" should "take the min and the max" in
  {
    assertClose(degree(And(t(0.3), t(0.7))), 0.3, "0.3 and 0.7")
    assertClose(degree(Or(t(0.3), t(0.7))), 0.7, "0.3 or 0.7")
    assertClose(degree(Not(t(0.3))), 0.7, "not 0.3")
  }

  it should "keep unknown as the negation fixpoint" in
  {
    assertClose(degree(Not(U)), 0.5)
  }

  "a graded degree" should "collapse to _Bool only at the endpoints" in
  {
    assert(_Truth.of(0.3).isInstanceOf[_Truth])
    assert(_Truth.of(1.0) == T)
    assert(And(t(0.3), F).eval(minmax) == Right(F))
  }

  // --- the three t-norms ---

  "the product t-norm" should "multiply and use the probabilistic sum" in
  {
    assertClose(degree(And(t(0.3), t(0.5)), product), 0.15, "0.3 and 0.5")
    assertClose(degree(Or(t(0.3), t(0.5)), product), 0.65, "0.3 or 0.5")   // 0.3+0.5-0.15
  }

  "the Lukasiewicz t-norm" should "use bounded difference and bounded sum" in
  {
    assertClose(degree(And(t(0.3), t(0.5)), lukas), 0.0, "0.3 and 0.5")     // max(0, -0.2)
    assertClose(degree(And(t(0.8), t(0.7)), lukas), 0.5, "0.8 and 0.7")     // 1.5 - 1
    assertClose(degree(Or(t(0.3), t(0.5)), lukas), 0.8, "0.3 or 0.5")
    assertClose(degree(Or(t(0.8), t(0.7)), lukas), 1.0, "0.8 or 0.7")       // min(1, 1.5)
  }

  "every semantics" should "have 1 as the t-norm identity and 0 as its annihilator" in
  {
    for env <- List(minmax, product, lukas); d <- List(0.0, 0.25, 0.5, 0.75, 1.0) do
      assertClose(degree(And(t(d), T), env), d, s"identity under ${env.semantics} at $d")
      assertClose(degree(And(t(d), F), env), 0.0, s"annihilator under ${env.semantics} at $d")
      assertClose(degree(Or(t(d), F), env), d, s"or-identity under ${env.semantics} at $d")
      assertClose(degree(Or(t(d), T), env), 1.0, s"or-annihilator under ${env.semantics} at $d")
  }

  it should "be monotone in each argument" in
  {
    for env <- List(minmax, product, lukas) do
      val rising = List(0.0, 0.2, 0.4, 0.6, 0.8, 1.0).map(d => degree(And(t(d), t(0.6)), env))
      assert(rising == rising.sorted, s"t-norm not monotone under ${env.semantics}: $rising")
      val risingOr = List(0.0, 0.2, 0.4, 0.6, 0.8, 1.0).map(d => degree(Or(t(d), t(0.6)), env))
      assert(risingOr == risingOr.sorted, s"t-conorm not monotone under ${env.semantics}: $risingOr")
  }

  it should "agree with classical logic on the crisp values" in
  {
    for env <- List(minmax, product, lukas); p <- List(true, false); q <- List(true, false) do
      assert(And(_Bool(p), _Bool(q)).eval(env) == Right(_Bool(p && q)), s"And($p,$q) / ${env.semantics}")
      assert(Or(_Bool(p), _Bool(q)).eval(env) == Right(_Bool(p || q)), s"Or($p,$q) / ${env.semantics}")
      assert(Implies(_Bool(p), _Bool(q)).eval(env) == Right(_Bool(!p || q)), s"Implies($p,$q) / ${env.semantics}")
      assert(Xor(_Bool(p), _Bool(q)).eval(env) == Right(_Bool(p != q)), s"Xor($p,$q) / ${env.semantics}")
  }

  "the short-circuits" should "stay sound under every semantics" in
  {
    val unreducible = parse("1/0 = 0")
    for env <- List(minmax, product, lukas) do
      assert(And(F, unreducible).eval(env) == Right(F), s"And short-circuit / ${env.semantics}")
      assert(Or(T, unreducible).eval(env) == Right(T), s"Or short-circuit / ${env.semantics}")
      assert(Implies(F, unreducible).eval(env) == Right(T), s"Implies short-circuit / ${env.semantics}")
  }

  "xor" should "agree with its (a and not b) or (not a and b) desugaring under every semantics" in
  {
    for env <- List(minmax, product, lukas); p <- List(0.0, 0.3, 0.5, 1.0); q <- List(0.0, 0.4, 1.0) do
      val direct    = degree(Xor(t(p), t(q)), env)
      val desugared = degree(Or(And(t(p), Not(t(q))), And(Not(t(p)), t(q))), env)
      assertClose(direct, desugared, s"xor($p,$q) / ${env.semantics}:")
  }

  // --- simplification is gated on the lattice property ---

  "idempotence and absorption" should "hold for graded operands only under min-max" in
  {
    val g = t(0.3)
    assert(simplifyLogicFully(And(g, g), identity, false, LogicSemantics.MinMax) == g)
    // under product, a and a = a^2, so the rule must not fire structurally; the
    // constant fold gives the correct 0.09 instead
    val folded = simplifyLogicFully(And(g, g), identity, false, LogicSemantics.Product)
    assert(folded != g, "product idempotence must not return the operand unchanged")
    folded match
      case _Truth(d) => assertClose(d, 0.09, "0.3 and 0.3 under product:")
      case other     => fail(s"expected a graded fold, got: $other")
  }

  it should "still hold for free (crisp-atom) variables under every semantics" in
  {
    for s <- LogicSemantics.values do
      assert(simplifyLogicFully(And(a, a), identity, false, s) == a, s"idempotence / $s")
      assert(simplifyLogicFully(Or(a, And(a, _Variable("b"))), identity, false, s) == a, s"absorption / $s")
  }

  it should "not absorb a graded operand outside min-max" in
  {
    val g = t(0.3)
    val e = Or(g, And(g, a))
    assert(simplifyLogicFully(e, identity, false, LogicSemantics.MinMax) == g)
    assert(simplifyLogicFully(e, identity, false, LogicSemantics.Product) == e)
  }

  "the identity and annihilator rules" should "fire under every semantics" in
  {
    for s <- LogicSemantics.values do
      assert(simplifyLogicFully(And(a, T), identity, false, s) == a, s"and-identity / $s")
      assert(simplifyLogicFully(And(a, F), identity, false, s) == F, s"and-annihilator / $s")
      assert(simplifyLogicFully(Or(a, F), identity, false, s) == a, s"or-identity / $s")
      assert(simplifyLogicFully(Not(Not(a)), identity, false, s) == a, s"double negation / $s")
  }

  // --- hedges ---

  "the hedges" should "concentrate and dilate a degree" in
  {
    assertClose(degree(Very(t(0.5))), 0.25, "very 0.5")
    assertClose(degree(Somewhat(t(0.25))), 0.5, "somewhat 0.25")
    assertClose(degree(Very(_Number(0.5))), 0.25, "very(0.5) on a bare number")
  }

  it should "be inverse of each other on the interior" in
  {
    assertClose(degree(Somewhat(Very(t(0.3)))), 0.3)
  }

  it should "fix the crisp endpoints" in
  {
    assert(Very(T).eval(minmax) == Right(T))
    assert(Very(F).eval(minmax) == Right(F))
    assert(Somewhat(T).eval(minmax) == Right(T))
  }

  it should "stay symbolic on an out-of-domain or unreduced argument" in
  {
    assert(Very(_Number(3.0)).eval(minmax).isLeft)
    assert(Very(a).eval(minmax).isLeft)
  }

  // --- membership curves ---

  "trimf" should "rise to the apex and fall back" in
  {
    def tri(v: Double) = degree(TriMF(_Number(v), _Number(0), _Number(5), _Number(10)))
    assertClose(tri(0.0), 0.0, "at the left foot")
    assertClose(tri(2.5), 0.5, "halfway up")
    assertClose(tri(5.0), 1.0, "at the apex")
    assertClose(tri(7.5), 0.5, "halfway down")
    assertClose(tri(10.0), 0.0, "at the right foot")
    assertClose(tri(-3.0), 0.0, "left of the support")
    assertClose(tri(20.0), 0.0, "right of the support")
  }

  it should "handle a degenerate shoulder without dividing by zero" in
  {
    // a == b: vertical left edge
    assertClose(degree(TriMF(_Number(0), _Number(0), _Number(0), _Number(10))), 1.0)
  }

  it should "stay symbolic when the feet are out of order" in
  {
    assert(TriMF(_Number(1), _Number(5), _Number(2), _Number(10)).eval(minmax).isLeft)
  }

  "trapmf" should "have a plateau of full membership" in
  {
    def trap(v: Double) =
      degree(TrapMF(_Number(v), _Number(0), _Number(2), _Number(8), _Number(10)))
    assertClose(trap(0.0), 0.0)
    assertClose(trap(1.0), 0.5)
    assertClose(trap(2.0), 1.0)
    assertClose(trap(5.0), 1.0, "on the plateau")
    assertClose(trap(8.0), 1.0)
    assertClose(trap(9.0), 0.5)
    assertClose(trap(10.0), 0.0)
  }

  "gaussmf" should "peak at the mean and fall off symmetrically" in
  {
    def g(v: Double) = degree(GaussMF(_Number(v), _Number(3), _Number(1)))
    assertClose(g(3.0), 1.0, "at the mean")
    assertClose(g(4.0), math.exp(-0.5), "one sigma out")
    assertClose(g(2.0), g(4.0), "symmetric")
  }

  it should "stay symbolic for a non-positive sigma" in
  {
    assert(GaussMF(_Number(1), _Number(0), _Number(0)).eval(minmax).isLeft)
    assert(GaussMF(_Number(1), _Number(0), _Number(-1)).eval(minmax).isLeft)
  }

  "sigmf" should "pass through one half at the inflection point" in
  {
    assertClose(degree(SigMF(_Number(5), _Number(2), _Number(5))), 0.5)
    assert(degree(SigMF(_Number(10), _Number(2), _Number(5))) > 0.9)
    assert(degree(SigMF(_Number(0), _Number(2), _Number(5))) < 0.1)
  }

  "membership curves" should "compose with the connectives directly" in
  {
    // a curve yields a truth value, so no explicit cast is needed
    val e = And(TriMF(_Number(5), _Number(0), _Number(5), _Number(10)), t(0.4))
    assertClose(degree(e), 0.4)
  }

  // --- truth(x) conversion and round-trip ---

  "truth(x)" should "convert a scalar degree into a truth value" in
  {
    assertClose(degree(_TruthOf(_Number(0.25))), 0.25)
    assert(_TruthOf(_Number(1.0)).eval(minmax) == Right(T))
  }

  it should "be the printed form of a graded degree" in
  {
    assert(_Truth.of(0.25).toString == "truth(0.25)")
    assert(U.toString == "unknown", "the Kleene midpoint keeps its own literal")
  }

  it should "make a graded degree round-trip through the parser" in
  {
    for d <- List(0.25, 0.3, 0.75) do
      val v = _Truth.of(d)
      val reparsed = parse(v.toString).eval(minmax)
      assert(reparsed == Right(v), s"round-trip failed for $d: $reparsed")
  }

  // --- parsing ---

  "the fuzzy grammar" should "parse the hedges and curves" in
  {
    assert(parse("very(0.5)") == Very(_Number(0.5)))
    assert(parse("somewhat(0.25)") == Somewhat(_Number(0.25)))
    assert(parse("trimf(x, 0, 5, 10)") == TriMF(x, _Number(0), _Number(5), _Number(10)))
    assert(parse("gaussmf(x, 3, 1)") == GaussMF(x, _Number(3), _Number(1)))
    assert(parse("sigmf(x, 2, 5)") == SigMF(x, _Number(2), _Number(5)))
    assert(parse("trapmf(x, 0, 2, 8, 10)") ==
      TrapMF(x, _Number(0), _Number(2), _Number(8), _Number(10)))
  }

  it should "compose curves under the connectives" in
  {
    assert(parse("very(0.5) and somewhat(0.25)") == And(Very(_Number(0.5)), Somewhat(_Number(0.25))))
    assertClose(degree(parse("very(0.5) and somewhat(0.25)")), 0.25)
  }

  it should "round-trip every fuzzy node through toString" in
  {
    val cases = List[_Expression](
      Very(_Number(0.5)), Somewhat(x), _TruthOf(_Number(0.25)),
      TriMF(x, _Number(0), _Number(5), _Number(10)),
      TrapMF(x, _Number(0), _Number(2), _Number(8), _Number(10)),
      GaussMF(x, _Number(3), _Number(1)), SigMF(x, _Number(2), _Number(5)),
      _Defuzzify(TriMF(x, _Number(0), _Number(5), _Number(10)), x, _Number(0), _Number(10)))
    for e <- cases do
      val printed  = e.toString
      val reparsed = Parser.parse(printed)
      assert(reparsed.successful, s"toString did not re-parse: \"$printed\" ($reparsed)")
      assert(reparsed.get == e, s"round-trip changed \"$printed\": ${reparsed.get}")
  }

  // --- defuzzification ---

  "the centroid of a symmetric triangle" should "be its apex" in
  {
    val tri = TriMF(x, _Number(0), _Number(5), _Number(10))
    centroid(tri, x, 0.0, 10.0) match
      case Some(c) => assertClose(c, 5.0, "centroid:")
      case None    => fail("expected a centroid")
  }

  "the centroid of a symmetric gaussian" should "be its mean" in
  {
    val g = GaussMF(x, _Number(3), _Number(1))
    centroid(g, x, 0.0, 6.0) match
      case Some(c) => assertClose(c, 3.0, "centroid:")
      case None    => fail("expected a centroid")
  }

  "the centroid of an asymmetric triangle" should "lean towards the heavier side" in
  {
    val tri = TriMF(x, _Number(0), _Number(2), _Number(10))
    centroid(tri, x, 0.0, 10.0) match
      case Some(c) => assert(c > 2.0 && c < 5.0, s"expected between the apex and the midpoint, got $c")
      case None    => fail("expected a centroid")
  }

  "meanOfMaxima" should "average the plateau of a trapezoid" in
  {
    val trap = TrapMF(x, _Number(0), _Number(2), _Number(8), _Number(10))
    meanOfMaxima(trap, x, 0.0, 10.0) match
      case Some(m) => assertClose(m, 5.0, "mean of maxima:")
      case None    => fail("expected a mean of maxima")
  }

  "bisector" should "split a symmetric curve at its centre" in
  {
    val tri = TriMF(x, _Number(0), _Number(5), _Number(10))
    bisector(tri, x, 0.0, 10.0) match
      case Some(b) => assert(math.abs(b - 5.0) <= 0.1, s"expected about 5.0, got $b")
      case None    => fail("expected a bisector")
  }

  "defuzzifying an all-zero curve" should "yield None" in
  {
    val tri = TriMF(x, _Number(100), _Number(105), _Number(110))
    assert(centroid(tri, x, 0.0, 10.0).isEmpty)
    assert(meanOfMaxima(tri, x, 0.0, 10.0).isEmpty)
    assert(bisector(tri, x, 0.0, 10.0).isEmpty)
  }

  "sampleDegrees" should "use the scalar compile fast path for a plain scalar expression" in
  {
    // x/10 is pure scalar arithmetic, so compile lowers it; the degrees still come out
    val pts = sampleDegrees(parse("x / 10"), x, 0.0, 10.0, 11)
    assert(pts.size == 11)
    assertClose(pts.head._2, 0.0)
    assertClose(pts.last._2, 1.0)
  }

  it should "fall back to tree evaluation for a truth-valued node" in
  {
    val pts = sampleDegrees(TriMF(x, _Number(0), _Number(5), _Number(10)), x, 0.0, 10.0, 11)
    assert(pts.size == 11)
    assertClose(pts(5)._2, 1.0, "apex:")
  }

  "the defuzz node" should "evaluate to the centroid and stay symbolic on free bounds" in
  {
    val tri = TriMF(x, _Number(0), _Number(5), _Number(10))
    _Defuzzify(tri, x, _Number(0), _Number(10)).eval(minmax) match
      case Right(_Number(c)) => assertClose(c, 5.0)
      case other             => fail(s"expected a number, got: $other")
    assert(_Defuzzify(tri, x, _Variable("lo"), _Number(10)).eval(minmax).isLeft)
    // an empty or inverted range stays symbolic
    assert(_Defuzzify(tri, x, _Number(10), _Number(0)).eval(minmax).isLeft)
  }

  // --- custom membership functions: the built-in curves are a convenience, not a limit ---

  "truth(<expr>)" should "define a custom membership curve from any scalar expression" in
  {
    // a Cauchy bell centred on 5, which is not one of the four built-in shapes
    val bell = parse("truth(1 / (1 + (x - 5)^2))")
    def at(v: Double): Double = degree(bell, minmax.withBinding("x", _Number(v)))
    assertClose(at(5.0), 1.0, "at the centre")
    assertClose(at(4.0), 0.5, "one unit out")
    assertClose(at(6.0), at(4.0), "symmetric")
    assert(at(0.0) < at(4.0), "decaying away from the centre")
  }

  it should "compose with the hedges and the connectives like a built-in curve" in
  {
    val env4 = minmax.withBinding("x", _Number(4.0))
    assertClose(degree(parse("very(truth(1 / (1 + (x - 5)^2)))"), env4), 0.25, "very of the custom curve")
    assertClose(degree(parse("truth(1 / (1 + (x - 5)^2)) and truth(0.2)"), env4), 0.2, "and:")
  }

  it should "defuzzify like a built-in curve" in
  {
    centroid(parse("truth(1 / (1 + (x - 5)^2))"), x, 0.0, 10.0) match
      case Some(c) => assertClose(c, 5.0, "custom centroid:")
      case None    => fail("expected a centroid for the custom curve")
  }

  it should "stay symbolic outside [0, 1] rather than clamping" in
  {
    assert(_TruthOf(_Number(2.0)).eval(minmax).isLeft)
    assert(_TruthOf(_Number(-1.0)).eval(minmax).isLeft)
  }

  "a raw scalar curve" should "defuzzify without the truth(...) wrapper" in
  {
    centroid(parse("1 / (1 + (x - 5)^2)"), x, 0.0, 10.0) match
      case Some(c) => assertClose(c, 5.0, "raw centroid:")
      case None    => fail("expected a centroid for the raw curve")
    // the compiled fast path and the tree path must agree on what counts as a degree:
    // values outside [0, 1] are not membership degrees and are dropped by both
    assert(centroid(parse("x"), x, 0.0, 10.0) == centroid(parse("truth(x)"), x, 0.0, 10.0),
      "the compile fast path and the tree path must agree")
  }

  "sampleDegrees" should "drop out-of-range points on the compiled fast path too" in
  {
    // x over [0, 10] is compilable but only x <= 1 is a degree
    val pts = sampleDegrees(parse("x"), x, 0.0, 10.0, 11)
    assert(pts.map(_._1) == Vector(0.0, 1.0), s"expected only the in-range points, got: $pts")
  }

  // --- aggregating curves with the connectives (the fuzzy-inference step) ---

  "a membership argument" should "accept connectives, so curves can be aggregated" in
  {
    // the union of two triangles, symmetric about 5 -> centroid back at 5
    parse("defuzz(trimf(x, 0, 3, 6) or trimf(x, 4, 7, 10), x, 0, 10)").eval(minmax) match
      case Right(_Number(c)) => assertClose(c, 5.0, "aggregated centroid:")
      case other             => fail(s"expected a number, got: $other")
  }

  it should "accept connectives under the hedges as well" in
  {
    assert(parse("very(trimf(x, 0, 5, 10) or trimf(x, 2, 6, 9))").isInstanceOf[Very])
    assert(parse("somewhat(truth(0.5) and truth(0.8))").isInstanceOf[Somewhat])
  }

  it should "not leak the logic level into ordinary function or factor positions" in
  {
    // only the membership positions moved up to the logic level; sin(...) and the
    // arithmetic paren branch still take an arithmetic argument, so a connective there
    // is a parse error rather than silently becoming a scalar operand
    assert(!Parser.parse("sin(a and b)").successful)
    assert(!Parser.parse("2 * (a and b)").successful)
    // connectives at the top level are of course still fine
    assert(Parser.parse("sin(a) and sin(b)").successful)
  }