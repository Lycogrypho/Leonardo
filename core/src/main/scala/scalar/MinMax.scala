package it.grypho.scala.leonardo
package scalar

import core.*


/** The order-based functions (F_0056): the element-wise join `maximum`/`minimum`, the reduction
 *  `max`/`min`, `clamp` and the smooth `softplus`.
 *
 *  **Two operations, two names** (Decision A, the numpy split): `maximum(a, b, …)` is the
 *  *join* — the larger argument, element-wise on matrices with a number spreading over every
 *  cell — while `max(v)` is the *reduction*, the largest entry of one array. One name chosen by
 *  arity (MATLAB) would make a one-argument call change the result type; two names keep each
 *  word one operation.
 *
 *  **What has an order.** Real numbers (`_Number`, `_Rational`, `_Based`), and matrices of them
 *  element by element. A complex value has none (the `_Comparison` rule); a truth value is
 *  declined because its join IS `or` under the min–max semantics and not under `product` or
 *  `lukasiewicz`, so a second spelling of it would silently disagree; a distribution's maximum
 *  is a new random variable. All of these stay symbolic. **A free name is never guessed** to be
 *  a scalar (F_0055 Decision C): it may later be bound to a matrix, so the node waits.
 *
 *  **A selection, not arithmetic**: the result is one of the operands, returned as it is, so an
 *  exact operand stays exact. **At a tie the first argument wins** — in value that is invisible,
 *  in the derivative it is the stated subgradient choice (see `Derive.scala`).
 */

/** Which end of the order an operation takes. */
enum Extreme:
  /** The largest. */
  case Max
  /** The smallest. */
  case Min

object Extreme:
  /** The keyword of the element-wise join. */
  def joinName(k: Extreme): String = k match
    case Max => "maximum"
    case Min => "minimum"
  /** The keyword of the reduction. */
  def reduceName(k: Extreme): String = k match
    case Max => "max"
    case Min => "min"


/** The order of two real values, or `None` when either has none — exact pairs compared exactly.
 *
 *  @param a the left value
 *  @param b the right value
 *  @return `-1`, `0` or `1`, or `None` for a complex, truth or symbolic operand
 */
private[leonardo] def compareReal(a: _Expression, b: _Expression): Option[Int] = (a, b) match
  case (x: _Rational, y: _Rational)                      => Some(x.subtract(y).signum)
  case (_Number(x), _Number(y)) if !x.isNaN && !y.isNaN => Some(if x < y then -1 else if x > y then 1 else 0)
  case _                                                 => None

/** The extreme of `vs`, the first winning a tie, or `None` when two cannot be ordered. */
private def pick[V <: _Expression](k: Extreme, vs: Seq[V]): Option[V] =
  vs.tail.foldLeft(Option(vs.head)) { (acc, b) =>
    acc.flatMap(a => compareReal(a, b).map(c => if (k == Extreme.Max && c < 0) || (k == Extreme.Min && c > 0) then b else a))
  }

/** One evaluated operand, by shape. */
private enum Operand:
  case Scalar(v: _Value)
  case Dense(m: _MatrixValue)
  case Shaped(m: _MatrixShaped)

/** An evaluated operand's shape, or `None` when it has no order or its shape is unknown. */
private def operandOf(r: Either[_Expression, _Value]): Option[Operand] = r match
  case Right(m: _MatrixValue)  => Some(Operand.Dense(m))
  case Left(m: _MatrixShaped)  => Some(Operand.Shaped(m))
  case Right(v @ _Number(_))   => Some(Operand.Scalar(v))
  case _                       => None

/** Evaluates an order-based node: numbers go to `scalar`, matrices element-wise with numbers
 *  spreading over every cell, anything else leaves the node unevaluated with its operands
 *  reduced.
 *
 *  Element-wise, each cell is the node rebuilt over that cell's operands and evaluated, so a
 *  symbolic cell stays symbolic on its own while the others fold. The result is built on the
 *  first symbolic matrix operand (so exact cells stay exact), or dense when every operand was.
 */
private def orderEval(node: _Expression, args: List[_Expression], env: Environment)
                     (scalar: List[_Value] => Option[_Value]): Either[_Expression, _Value] =
  val reduced = args.map(_.eval(env))
  lazy val unevaluated = Left(node.rebuild(reduced.map(_.toExpression)))
  val ops = reduced.flatMap(operandOf)
  if ops.size != reduced.size then unevaluated
  else
    val shapes = ops.collect { case Operand.Dense(m) => (m.rows, m.cols); case Operand.Shaped(m) => (m.rows, m.cols) }
    if shapes.isEmpty then scalar(ops.collect { case Operand.Scalar(v) => v }).map(Right(_)).getOrElse(unevaluated)
    else if shapes.distinct.size > 1 then unevaluated
    else
      val (rows, cols) = shapes.head
      val cells = Vector.tabulate(rows * cols) { k =>
        node.rebuild(ops.map {
          case Operand.Scalar(v) => v
          case Operand.Dense(m)  => _Number(m.toVector(k))
          case Operand.Shaped(m) => m.children(k)
        }).eval(env).toExpression
      }
      ops.collectFirst { case Operand.Shaped(m) => m } match
        case Some(template) => template.rebuild(cells.toList).eval(env)
        case None =>
          // Every operand was dense: the cells are numbers (an exact scalar spread over a dense
          // matrix is read inexact, as the dense operand already is).
          val ds = cells.collect { case _Number(d) => d }
          if ds.size == cells.size then _MatrixValue(rows, cols, ds.toArray).guarded(node) else unevaluated


/** The element-wise join: `maximum(a, b, …)` / `minimum(a, b, …)`.
 *
 *  @param kind which end of the order
 *  @param args two or more operands; a number spreads over a matrix operand's cells
 */
case class _Join(kind: Extreme, args: List[_Expression]) extends NamedFunction:
  override def name: String = Extreme.joinName(kind)
  override def children: List[_Expression] = args
  override def rebuild(c: List[_Expression]): _Expression = _Join(kind, c)
  override def eval(env: Environment): Either[_Expression, _Value] =
    orderEval(this, args, env)(vs => pick(kind, vs))

/** The reduction: `max(v)` / `min(v)`, the extreme entry of one array.
 *
 *  Over **every** entry, row-major, whatever the shape — the `mean(x)` sample convention, not
 *  MATLAB's column-wise rule. A number is its own extreme.
 *
 *  @param kind which end of the order
 *  @param arg  the array, or a number
 */
case class _Extreme(kind: Extreme, arg: _Expression) extends NamedFunction:
  override def name: String = Extreme.reduceName(kind)
  override def children: List[_Expression] = List(arg)
  override def rebuild(c: List[_Expression]): _Expression = _Extreme(kind, c.head)
  override def eval(env: Environment): Either[_Expression, _Value] =
    arg.eval(env) match
      case Right(v @ _Number(_))   => Right(v)
      case Right(m: _MatrixValue)  => pick(kind, m.toVector.map(_Number(_))).map(Right(_)).getOrElse(Left(this))
      case r @ Left(m: _MatrixShaped) =>
        val values = m.children.collect { case v: _Value => v }
        (if values.size == m.children.size then pick(kind, values) else None)
          .map(Right(_)).getOrElse(Left(_Extreme(kind, r.toExpression)))
      case r => Left(_Extreme(kind, r.toExpression))

/** Saturation: `clamp(x, lo, hi)`, `x` limited to `[lo, hi]`, element-wise with numbers spreading
 *  (actuator saturation of an input column, against scalar or column bounds). `lo > hi` has no
 *  interval to clamp into and leaves it unevaluated.
 *
 *  @param x  the value(s) to limit
 *  @param lo the lower bound(s)
 *  @param hi the upper bound(s)
 */
case class _Clamp(x: _Expression, lo: _Expression, hi: _Expression) extends NamedFunction:
  override def name: String = "clamp"
  override def children: List[_Expression] = List(x, lo, hi)
  override def rebuild(c: List[_Expression]): _Expression = _Clamp(c.head, c(1), c(2))
  override def eval(env: Environment): Either[_Expression, _Value] =
    orderEval(this, children, env) {
      case List(v, l, h) =>
        for
          lh <- compareReal(l, h) if lh <= 0
          vl <- compareReal(v, l)
          vh <- compareReal(v, h)
        yield if vl < 0 then l else if vh > 0 then h else v
      case _ => None
    }

/** The smooth `max(0, x)`: `softplus(x, k) = ln(1 + e^(k·x))/k`, tending to `max(0, x)` as `k`
 *  grows. `k` must be positive.
 *
 *  **Never evaluated as written**: `ln(1 + e^(k·x))` overflows once `k·x` passes ~709, so the
 *  value is `(max(k·x, 0) + log1p(e^(−|k·x|)))/k` — the numerical conditioning rule. An exact
 *  argument re-enters the exact tier through `fromApproximation`, the transcendental precedent.
 *
 *  @param x the argument(s), element-wise
 *  @param k the sharpness, positive
 */
case class _Softplus(x: _Expression, k: _Expression) extends NamedFunction:
  override def name: String = "softplus"
  override def children: List[_Expression] = List(x, k)
  override def rebuild(c: List[_Expression]): _Expression = _Softplus(c.head, c(1))
  override def eval(env: Environment): Either[_Expression, _Value] =
    orderEval(this, children, env) {
      case List(xv @ _Number(xd), kv @ _Number(kd)) if kd > 0 =>
        val d = softplusOf(xd, kd)
        if xv.isInstanceOf[_Rational] || kv.isInstanceOf[_Rational] then _Rational.fromApproximation(d, env.workingPrecision)
        else Some(_Number(d))
      case _ => None
    }

/** `softplus` in its overflow-free form. */
private[scalar] def softplusOf(x: Double, k: Double): Double =
  val z = k * x
  (math.max(z, 0) + math.log1p(math.exp(-math.abs(z)))) / k
