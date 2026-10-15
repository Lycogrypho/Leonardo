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
