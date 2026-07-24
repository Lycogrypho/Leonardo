package it.grypho.scala.leonardo
package core


/** Base trait of every expression tree node, shared across all domains.
 *
 *  The dual-evaluation contract: `eval` returns `Right` when the expression fully reduces
 *  to a concrete [[_Value]]; `Left` when some variable is still free and the node is
 *  returned in its most-reduced symbolic form.
 *
 *  `children` / `rebuild` enable generic structural traversal so algorithms that visit
 *  every node type (`Substitute`, `Analysis`, …) do not need to match each case explicitly.
 *  Binder positions (e.g. the differentiation variable) are excluded from `children`;
 *  `rebuild` carries them through unchanged.
 *
 *  `freeVars` is cached per instance — computed once from `children`, then O(1).
 *  New node types inherit a correct default automatically.
 */
trait _Expression:
  /** Reduces this expression in the given environment.
   *  @param env variable bindings and display precision
   *  @return `Right(v)` when all free variables resolved to concrete values;
   *          `Left(e)` when reduction is partial or impossible
   */
  def eval(env: Environment): Either[_Expression, _Value]

  /** Sub-expressions subject to recursive structural traversal.
   *  Binder positions (e.g. the differentiation variable) are excluded.
   */
  def children: List[_Expression]

  /** Reconstructs the same node shape with replacement sub-expressions.
   *  @param newChildren replacements in the same order and count as `children`
   */
  def rebuild(newChildren: List[_Expression]): _Expression

  /** Cached set of free variable names; O(1) after the first access. */
  lazy val freeVars: Set[String] = children.flatMap(_.freeVars).toSet


/** Marker trait for a fully-reduced, concrete result — a number, a matrix, or a boolean —
 *  as opposed to a symbolic atom (free variable) that is not yet concrete.
 */
trait _Value extends _Expression


/** Marker trait for expression nodes whose children may receive per-element algorithm
 *  passes (derive, simplify, expand, integrate).
 *
 *  An algorithm may distribute over `children` and `rebuild` the same shape only when this
 *  trait is present **and** the distribution is mathematically valid for *all* such
 *  algorithms.  Linear containers qualify; product-like nodes that require product rules
 *  must not be marked.  Lives in `core` so domain packages can opt in without creating a
 *  cross-domain import.
 */
trait _ElementWise extends _Expression


/** Marker trait for a symbolic matrix node whose `children` are its cells in row-major
 *  order and whose `rebuild` preserves the `rows × cols` shape.
 *
 *  Lets `core` and `scalar` algorithms distribute a scalar function element-wise over a
 *  symbolic matrix argument (`exp(A)`, `sin(A)`, … over a matrix with free-variable cells)
 *  without importing the `matrix` package — they see only this marker and the generic
 *  `children` / `rebuild`.  Narrower than [[_ElementWise]] on purpose: only the matrix
 *  literal opts in.  The dense counterpart ([[_MatrixValue]]) is handled numerically and
 *  is not marked.
 */
trait _MatrixShaped extends _Expression:
  /** Number of rows in the symbolic matrix. */
  def rows: Int
  /** Number of columns in the symbolic matrix. */
  def cols: Int


/** Companion for the concrete real scalar value [[_Number]]. */
object _Number:
  private val factorTable: Array[Double] = Array.tabulate(16)(i => scala.math.pow(10.0, i))

  /** Rounds `d` to `precision` decimal places for display; returns `d` unchanged when it
   *  is NaN, infinite, or too large for the rounded value to fit in a `Long`.
   */
  private[core] def round(d: Double, precision: Int): Double =
    if d.isNaN || d.isInfinite then d
    else
      val factor = if precision >= 0 && precision < factorTable.length then factorTable(precision)
                   else scala.math.pow(10, precision)
      // guard: d * factor must fit in Long, otherwise rounding is meaningless
      if scala.math.abs(d) * factor > Long.MaxValue.toDouble then d
      else (d * factor).round.toDouble / factor

/** Concrete real scalar value.
 *
 *  Rounding is a display concern only: `toString` and `display` round for output; `eval`
 *  propagates the stored `Double` as-is.  `±∞` serialises as `"inf"` / `"-inf"` for
 *  round-trip safety through the parser.
 *
 *  @param d the exact double-precision value
 */
case class _Number(d: Double) extends _Value:
  /** Renders this number at [[Environment.DefaultPrecision]] decimal places.
   *  `±∞` renders as `"inf"` / `"-inf"` for round-trip safety.
   */
  override def toString: String =
    if d.isPosInfinity then "inf"
    else if d.isNegInfinity then "-inf"
    else _Number.round(d, Environment.DefaultPrecision).toString

  /** Renders this number at `precision` decimal places for REPL display.
   *  @param precision number of decimal places to show
   */
  def display(precision: Int): String =
    if d.isPosInfinity then "inf"
    else if d.isNegInfinity then "-inf"
    else _Number.round(d, precision).toString

  /** Returns `Right(this)` — a concrete number needs no further reduction. */
  override def eval(env: Environment): Either[_Expression, _Value] = Right(this)

  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this


/** Concrete boolean value — the result of a fully-reduced relation.
 *  @param b the truth value
 */
case class _Bool(b: Boolean) extends _Value:
  override def toString: String = b.toString
  /** Returns `Right(this)` — a concrete boolean needs no further reduction. */
  override def eval(env: Environment): Either[_Expression, _Value] = Right(this)
  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this


/** A free variable: a symbolic atom that is not yet a concrete value.
 *
 *  `eval` returns `Right` only when `variable` is bound to a [[_Value]] in the environment;
 *  otherwise it stays `Left(this)`.
 *
 *  @param variable the variable name
 */
case class _Variable(variable: String) extends _Expression:
  override def toString: String = variable

  /** Looks up this variable in `env` and delegates `eval` to the bound value if found.
   *  @param env variable bindings; this variable's name is the lookup key
   */
  override def eval(env: Environment): Either[_Expression, _Value] =
    env.get(variable) match
      case Some(n) => n.eval(env)
      case None    => Left(this)

  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this
  override lazy val freeVars: Set[String] = Set(variable)


extension (result: Either[_Expression, _Value])
  /** Collapses an `eval` result back to a plain [[_Expression]].
   *  Used when rebuilding a symbolic node from partially-reduced operands.
   */
  def toExpression: _Expression = result match
    case Left(e)  => e
    case Right(v) => v
