package it.grypho.scala.leonardo
package core


/** One configurable work or size budget (F_0124): a cap on how much one call may compute before
 *  it declines rather than answer.
 *
 *  **A budget, never a tolerance.** Each bounds time or memory: a basis's size, an iteration
 *  count, a problem's dimension. None decides what counts as numerically zero, so raising one
 *  can only let a slow case finish, never make an answer less certain: every answer it guards
 *  is still certified by its own method. The tolerances stay fixed and relative; the one a
 *  caller legitimately sets, the certificate's, comes from [[Environment.precision]].
 *
 *  The default is the value the library has always used, measured for the browser REPL where a
 *  frozen tab cannot be interrupted (the inventory is in `DesignNotes.md`, F_0124). A library
 *  caller with time to spare raises it through [[Environment.withLimit]]. Each case is named
 *  after the published constant it backs (`optimize.MaxQpVariables` is
 *  `Limit.MaxQpVariables.default`), so the two cannot disagree.
 *
 *  @param default the value used unless an [[Environment]] overrides it
 */
enum Limit(val default: Long):

  // ── optimize ──

  /** Iterations of either QP method before it declines. */
  case MaxQpIterations extends Limit(2000)
  /** Variables a QP may have. */
  case MaxQpVariables extends Limit(500)
  /** Constraint rows, bounds included, a QP may have. */
  case MaxQpConstraints extends Limit(2000)
  /** Variables of an exact linear program. */
  case MaxLpVariables extends Limit(40)
  /** Constraints of an exact linear program. */
  case MaxLpConstraints extends Limit(80)
  /** Simplex pivots before an LP declines. */
  case MaxLpPivots extends Limit(20000)
  /** The widest coefficient, in bits, an exact simplex pivot may write. */
  case MaxLpBits extends Limit(2048)
  /** The arithmetic an exact linear program may do, in the units `optimize.MaxLpWork` defines (F_0115). */
  case MaxLpWork extends Limit(125000000)
  /** Iterations of `minimize`. */
  case MaxMinimizeIterations extends Limit(2000)
  /** SQP iterations of `nlp`. */
  case MaxNlpIterations extends Limit(200)
  /** Augmented-Lagrangian rounds of `nlp`. */
  case MaxAugLagRounds extends Limit(50)
  /** Inner iterations of one augmented-Lagrangian round. */
  case MaxAugLagInnerIterations extends Limit(500)
  /** Variables of a nonlinear program. */
  case MaxNlpVariables extends Limit(100)
  /** Levenberg–Marquardt iterations of `lsqfit`. */
  case MaxLsqIterations extends Limit(100)
  /** `m·n²` of a least-squares fit: its residuals times its parameters squared. */
  case MaxLsqWork extends Limit(400000)
  /** Variables `convex` examines (it checks `2ⁿ` principal minors). */
  case MaxConvexityVariables extends Limit(6)
  /** Inequalities `kktPoints` enumerates active sets over (`2ᵐ` of them). */
  case MaxKKTInequalities extends Limit(10)
  /** Steps, case splits included, of `eliminate`. */
  case MaxEliminationSteps extends Limit(4000)
  /** Elements a Gröbner basis may reach. */
  case MaxGroebnerBasis extends Limit(24)
  /** Total degree a Gröbner basis element may reach. */
  case MaxGroebnerDegree extends Limit(24)
  /** The widest coefficient, in bits, a Gröbner reduction may write. */
  case MaxGroebnerCoefficientBits extends Limit(4096)
  /** S-polynomial reductions of one Gröbner basis. */
  case MaxGroebnerReductions extends Limit(2000)

  // ── matrix, control, ode, probability (F_0124 phase B) ──

  /** Cells an assembled matrix may have: block operations, `mpcMatrices`, `dlsim`'s outputs. */
  case MaxMatrixCells extends Limit(1000000)
  /** `max(m, n)·min(m, n)²` of one singular value decomposition. */
  case MaxSvdWork extends Limit(10000000)
  /** One dimension of `eye`, `zeros` and `ones`. */
  case MaxDenseDim extends Limit(4096)
  /** The order of a symbolic matrix whose `det` and `inv` expand by cofactors (`n!` terms). */
  case MaxSymbolicDim extends Limit(6)
  /** Multiply-adds of one `dlsim`. */
  case MaxDlsimWork extends Limit(100000000)
  /** The prediction horizon of `mpcMatrices`. */
  case MaxMpcHorizon extends Limit(1000)
  /** The state (and output) dimension of the Riccati family. */
  case MaxRiccatiOrder extends Limit(200)
  /** Runge–Kutta steps of one ODE solve, scalar or vector. */
  case MaxOdeSteps extends Limit(1000000)
  /** Rows of one `odeSolve` trajectory. */
  case MaxTrajectoryRows extends Limit(100000)
  /** Draws of one `draw`. */
  case MaxDrawCount extends Limit(100000)
  /** Terms of one `prbs`. */
  case MaxPrbsLength extends Limit(1000000)

  // ── scalar, logic, equation, intervals (F_0124 phase C) ──

  /** Term pairs one multiplication inside `expand` may form. */
  case MaxExpandWork extends Limit(250000)
  /** The largest integer power `expand` multiplies out. */
  case MaxExpandPower extends Limit(20)
  /** The order of a Taylor, Laurent or Padé expansion (`m + n` for the last two). */
  case MaxTaylorOrder extends Limit(20)
  /** The order of a Fourier series. */
  case MaxFourierOrder extends Limit(20)
  /** Nodes one derivative may have on the series engine's fallback path. */
  case MaxDerivativeNodes extends Limit(50000)
  /** The largest `n` whose factorial or multifactorial is computed exactly. */
  case MaxExactFactorial extends Limit(10000)
  /** The largest index of an exact sequence term: `fib`, `binom`, `catalan`, `harmonic`. */
  case MaxExactSequenceIndex extends Limit(10000)
  /** Bits an exact rational power's operand may reach, estimated before it is computed. */
  case MaxExactPowerBits extends Limit(1048576)
  /** Terms of one `tabulate`, `sum` or `product`. */
  case MaxTabulateTerms extends Limit(1000)
  /** The largest integer exponent an interval is raised to. */
  case MaxIntervalPower extends Limit(100000)
  /** Variables of a boolean truth table (`2ⁿ` rows). */
  case MaxTruthTableVars extends Limit(16)
  /** Variables of a three-valued truth table (`3ⁿ` rows). */
  case MaxKleeneTableVars extends Limit(10)
  /** Clauses a CNF or DNF may reach while distributing. */
  case MaxNormalFormClauses extends Limit(1024)
  /** L'Hôpital steps a limit may take. */
  case MaxHopitalSteps extends Limit(5)
  /** Roots `solve`'s numeric fallback keeps. */
  case MaxNumericRoots extends Limit(8)
  /** The vectorised dimension of a linear matrix equation `solve` reduces by Kronecker products. */
  case MaxVectorizedSize extends Limit(400)

  // ── transforms (F_0137) ──

  /** The length of a `dft`/`idft`, by the direct `N²` sum; `DftTest` bounds the time at it. */
  case MaxDftLength extends Limit(8192)
  /** The products `n·m` a floating-point `conv` may form (F_0144), set by **Node**: at the cap,
   *  5000 × 5000, 0.78 s there against 32 ms on the JVM; `10⁸` took 4.1 s on Node and 194 ms on
   *  the JVM.  The output's own size is `MaxMatrixCells`. */
  case MaxConvWork extends Limit(25000000)
  /** The products `n·m` an exact `conv` may form (F_0144): sums of fractions grow their
   *  denominators, so the cost per product grows with the length.  Measured on distinct
   *  denominators: at the cap, 200 × 200, 0.42 s on Node and 92 ms on the JVM; 300 × 300, 1.9 s and
   *  0.70 s; 1000 × 1000, 170 s and 29 s. */
  case MaxConvExactWork extends Limit(40000)

  // ── matrices, continued (F_0176) ──

  /** The order of a matrix whose exact Jordan form `jordan` computes over the rationals (F_0176);
   *  past it an exact matrix takes the numeric path.  Measured on dense integer matrices `LJL⁻¹`
   *  with entries up to ±9 and several blocks: Node 67 ms at 16, 0.74 s at 20, 2.45 s at 24 and
   *  17.8 s at 32; the JVM 0.67 s at 20 and 4.4 s at 32. */
  case MaxJordanOrder extends Limit(20)
  /** The vectorised dimension `p·q` of a linear matrix equation `solve` reduces over the rationals
   *  (F_0178): Gauss–Jordan on the Kronecker system fills in large fractions.  Measured on dense
   *  integer `k×k` Sylvester equations: at 36 (`k = 6`) 0.62 s on the JVM and 1.0 s on Node, at 64
   *  2.0 s and 7.0 s, at 100 10.5 s and 55.6 s.  Past it an exact equation stays unevaluated. */
  case MaxExactVectorizedSize extends Limit(36)


/** The budgets an evaluation runs under (F_0124): each [[Limit]]'s default, unless overridden.
 *
 *  Carried by [[Environment]], so every node reaches it through `eval` and every library
 *  function through its `env`. **Not a case class**, on purpose: a record with a field per
 *  budget could not gain one without a binary break, while a new [[Limit]] case is additive.
 */
final class Limits private (private val overrides: Map[Limit, Long]):

  /** The value of `limit` here.
   *  @param limit the budget
   *  @return its override, or its default */
  def apply(limit: Limit): Long = overrides.getOrElse(limit, limit.default)

  /** The value of `limit` as an `Int`, saturating at `Int.MaxValue`.
   *  @param limit the budget
   *  @return its value, for a site that counts in `Int` */
  def count(limit: Limit): Int = math.min(apply(limit), Int.MaxValue.toLong).toInt

  /** These limits with `limit` set to `value`.
   *  @param limit the budget
   *  @param value its new value, at least 1
   *  @return the updated limits
   *  @throws IllegalArgumentException when `value < 1`: a budget of nothing would refuse every
   *          call, which no caller means */
  def withLimit(limit: Limit, value: Long): Limits =
    require(value >= 1, s"$limit must be at least 1, not $value")
    new Limits(if value == limit.default then overrides - limit else overrides.updated(limit, value))

  /** The budgets set away from their defaults.
   *  @return each overridden limit with its value */
  def overridden: Map[Limit, Long] = overrides

  override def equals(that: Any): Boolean = that match
    case l: Limits => l.overrides == overrides
    case _         => false

  override def hashCode: Int = overrides.hashCode

  override def toString: String =
    if overrides.isEmpty then "Limits(defaults)"
    else overrides.toList.sortBy(_._1.ordinal).map((l, v) => s"$l = $v").mkString("Limits(", ", ", ")")

/** The default budgets. */
object Limits:
  /** Every [[Limit]] at its default. */
  val Default: Limits = new Limits(Map.empty)
