package it.grypho.scala.leonardo
package core


// Foundation of every expression tree, shared across all domains.
// eval reduces an expression: Right holds a fully-reduced _Value; Left holds a
// symbolic expression that could not be fully reduced.
//
// children / rebuild: generic structural traversal — algorithms that touch every node
// type (Substitute, Analysis, …) use these instead of matching each case explicitly.
// children returns sub-expressions subject to recursive traversal; binder positions
// (e.g. the variable of differentiation in _Derivative) are excluded. rebuild(cs)
// reconstructs the same node shape with new sub-expressions.
//
// freeVars: the set of variable names that appear free in this expression. Cached
// as a lazy val per instance — computed once, then O(1). Provided with a default
// implementation in terms of children so new node types get it for free.
trait _Expression:
  def eval(env: Environment): Either[_Expression, _Value]
  def children: List[_Expression]
  def rebuild(newChildren: List[_Expression]): _Expression
  lazy val freeVars: Set[String] = children.flatMap(_.freeVars).toSet


// Marker for a fully-reduced, concrete result — a number now, a matrix/boolean/… later.
// Distinct from a symbolic atom such as a free variable.
trait _Value extends _Expression


// Marker for plain containers of expressions (a matrix literal, an element-wise sum,
// a transpose): per-element algorithms (derive, simplify, expand, integrate) may
// distribute over children and rebuild the same node shape. Mark a node ONLY when
// that distribution is mathematically valid for ALL such algorithms — linear
// containers qualify; product-like nodes, which need product rules, do not.
// Lives in core so domain packages can opt in without scalar importing them.
trait _ElementWise extends _Expression


object _Number:
  private val factorTable: Array[Double] = Array.tabulate(16)(i => scala.math.pow(10.0, i))

  private[core] def round(d: Double, precision: Int): Double =
    if d.isNaN || d.isInfinite then d
    else
      val factor = if precision >= 0 && precision < factorTable.length then factorTable(precision)
                   else scala.math.pow(10, precision)
      // guard: d * factor must fit in Long, otherwise rounding is meaningless
      if scala.math.abs(d) * factor > Long.MaxValue.toDouble then d
      else (d * factor).round.toDouble / factor

case class _Number(d: Double) extends _Value:
  // toString rounds for display only (DefaultPrecision = 5); no rounding in eval.
  override def toString: String = _Number.round(d, Environment.DefaultPrecision).toString
  // display(p) rounds to p decimal places — used by the REPL to respect session precision.
  def display(precision: Int): String = _Number.round(d, precision).toString

  // No rounding in eval: the stored Double is propagated as-is. Rounding is a
  // display concern only, handled at the boundary by toString / display(p).
  override def eval(env: Environment): Either[_Expression, _Value] = Right(this)

  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this


// A free variable is a symbolic atom, not a concrete value, so it is not a _Value:
// eval yields Right only once the variable is bound to one in the environment.
case class _Variable(variable: String) extends _Expression:
  override def toString: String = variable

  override def eval(env: Environment): Either[_Expression, _Value] =
    env.get(variable) match
      case Some(n) => n.eval(env)
      case None    => Left(this)

  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this
  override lazy val freeVars: Set[String] = Set(variable)


// Collapse an eval result back to a plain expression — used when rebuilding a
// symbolic node from partially-reduced operands.
extension (result: Either[_Expression, _Value])
  def toExpression: _Expression = result match
    case Left(e)  => e
    case Right(v) => v
