package it.grypho.scala.leonardo
package control

import core.*
import matrix.{_Matrix, MaxMatrixCells}


/** Why [[dlsim]] declined (F_0052 Decision A: the reason travels as data, the REPL prints it). */
enum SimFailure:
  /** The matrices or the data do not conform: `A` `n×n`, `B` `n×m`, `C` `p×n`, `D` `p×m`, `u`
   *  `m×N` (or `N×m`), `x0` `n×1`. */
  case ShapeMismatch(detail: String)
  /** An operand is not numeric — a symbolic entry, or not a matrix at all. */
  case NotNumeric(what: String)
  /** The simulation is past the size the library runs: [[matrix.MaxMatrixCells]] for an
   *  output, or [[MaxDlsimWork]] for the recursion. */
  case TooLarge(detail: String)
  /** A state or output stopped being a finite number at step `k` — an unstable model run long
   *  enough overflows, and `Infinity` is not an answer. */
  case Overflow(k: Int)

import SimFailure.*


/** The most multiply-adds [[dlsim]] runs, `N·(n² + n·m + p·n + p·m)`, checked before any work.
 *
 *  **Measured** (2026-10-09) just under the cap, at `9.1–9.4·10⁷`: 48–50 ms on the JVM and
 *  250–320 ms on Node, the slowest platform (n = 100, N = 9000 and n = 300, N = 1000). The output
 *  and state cells, capped at [[matrix.MaxMatrixCells]], bind first for most long simulations. */
val MaxDlsimWork: Long = 100_000_000L

/** Simulates the discrete-time model `x[k+1] = A·x[k] + B·u[k]`, `y[k] = C·x[k] + D·u[k]` over
 *  the input sequence `u` from `x[0] = x0` (F_0064, MPC T2-07).
 *
 *  Returns the outputs `Y` (`p×N`, column `k` is `y[k]`) and the states `X` (`n×(N+1)`, columns
 *  `x[0] … x[N]`). **The final state is included** where MATLAB's and scipy's `dlsim` stop at
 *  `x[N−1]`: `x[N]` is the state a simulation continues from, so returning it is the
 *  "state in, state out" rule (X-04) — a long run can be split into calls with no gap.
 *
 *  **Declines rather than overflowing**: an unstable model run long enough leaves the finite
 *  numbers, and the step where that happened is reported instead of a matrix of `Infinity`.
 *  Floating point throughout, the `expm`/`c2dExact` precedent.
 *
 *  @param a  the state matrix, `n×n`
 *  @param b  the input matrix, `n×m`
 *  @param c  the output matrix, `p×n`
 *  @param d  the feedthrough matrix, `p×m`
 *  @param u  the inputs, `m×N` — column `k` is `u[k]`
 *  @param x0 the initial state, `n×1`
 *  @return `(Y, X)`, or the reason the simulation was declined
 */
def dlsim(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, d: _MatrixValue,
          u: _MatrixValue, x0: _MatrixValue): Either[SimFailure, (_MatrixValue, _MatrixValue)] =
  val (n, m, p, steps) = (a.rows, b.cols, c.rows, u.cols)
  val work = steps.toLong * (n.toLong * n + n.toLong * m + p.toLong * n + p.toLong * m)
  if a.cols != n then Left(ShapeMismatch(s"A must be square: it is ${n}x${a.cols}"))
  else if b.rows != n then Left(ShapeMismatch(s"B must have $n rows, as A does: it is ${b.rows}x$m"))
  else if c.cols != n then Left(ShapeMismatch(s"C must have $n columns, as A does: it is ${p}x${c.cols}"))
  else if d.rows != p || d.cols != m then Left(ShapeMismatch(s"D must be ${p}x$m: it is ${d.rows}x${d.cols}"))
  else if u.rows != m then Left(ShapeMismatch(s"u must have $m rows, one per input: it is ${u.rows}x$steps"))
  else if x0.rows != n || x0.cols != 1 then Left(ShapeMismatch(s"x0 must be ${n}x1: it is ${x0.rows}x${x0.cols}"))
  else if p.toLong * steps > MaxMatrixCells || n.toLong * (steps + 1) > MaxMatrixCells then
    Left(TooLarge(s"the outputs or states would exceed $MaxMatrixCells cells"))
  else if work > MaxDlsimWork then Left(TooLarge(s"the recursion would take $work multiply-adds, past $MaxDlsimWork"))
  else run(a.toVector.toArray, b.toVector.toArray, c.toVector.toArray, d.toVector.toArray,
           u.toVector.toArray, x0.toVector.toArray, n, m, p, steps)

/** The recursion itself, over row-major arrays; `while` loops, since a boxed fold per step was
 *  measured ~8× slower in the QP's inner loops. */
private def run(a: Array[Double], b: Array[Double], c: Array[Double], d: Array[Double],
                u: Array[Double], x0: Array[Double], n: Int, m: Int, p: Int, steps: Int)
    : Either[SimFailure, (_MatrixValue, _MatrixValue)] =
  val ys = new Array[Double](p * steps)
  val xs = new Array[Double](n * (steps + 1))
  var x  = x0.clone
  var k  = 0
  var overflow = -1
  def put(state: Array[Double], col: Int): Unit =
    var i = 0
    while i < n do { xs(i * (steps + 1) + col) = state(i); i += 1 }
  put(x, 0)
  while k < steps && overflow < 0 do
    var i = 0
    while i < p do
      var acc = 0.0
      var j = 0
      while j < n do { acc += c(i * n + j) * x(j); j += 1 }
      var l = 0
      while l < m do { acc += d(i * m + l) * u(l * steps + k); l += 1 }
      ys(i * steps + k) = acc
      if !acc.isFinite then overflow = k
      i += 1
    val next = new Array[Double](n)
    i = 0
    while i < n do
      var acc = 0.0
      var j = 0
      while j < n do { acc += a(i * n + j) * x(j); j += 1 }
      var l = 0
      while l < m do { acc += b(i * m + l) * u(l * steps + k); l += 1 }
      next(i) = acc
      if !acc.isFinite && overflow < 0 then overflow = k + 1
      i += 1
    x = next
    put(x, k + 1)
    k += 1
  if overflow >= 0 then Left(Overflow(overflow))
  else Right((_MatrixValue(p, steps, ys), _MatrixValue(n, steps + 1, xs)))


/** A model matrix of the [[_Dlsim]] node: a matrix, `0` for an all-zero `rows×cols` block (the
 *  absent-group convention, for a `D` with no feedthrough), or another number as a `1×1` matrix
 *  (the `mpcMatrices` reading). */
private def simMatrix(r: Either[_Expression, _Value], what: String, zeroShape: => Option[(Int, Int)])
    : Either[SimFailure, _MatrixValue] =
  r match
    case Right(_Number(0.0)) if zeroShape.isEmpty => Left(ShapeMismatch(s"$what is 0, but its shape cannot be inferred"))
    case other => matrixOperand(other, zeroShape).toRight(NotNumeric(s"$what is not a numeric matrix"))

/** The four evaluated cells of a `[[A, B, C, D]]` model row, as `stateSpace`, `linearize` and
 *  `arx2ss` build it (F_0064, F_0065).
 *
 *  **Two carriers**: a row of matrices stays a symbolic `_Matrix`, but a scalar plant written
 *  with plain numbers — `stateSpace(0.5, 1, 1, 0)` — collapses to a dense `1×4` `_MatrixValue`,
 *  and was declined by `dlsim` as "not the row stateSpace builds" until F_0065 found it.
 *
 *  @param sys the model expression
 *  @param env the evaluation environment
 *  @return the four cells, or `None` when `sys` is not a `1×4` row
 */
private[control] def modelRow(sys: _Expression, env: Environment): Option[Vector[Either[_Expression, _Value]]] =
  sys.eval(env) match
    // Every cell of a collapsed row is a 1×1 matrix -- a 0 there is [[0]], not a zero block of
    // unknown shape, so the cells are rebuilt as matrices rather than handed on as numbers.
    case Right(m: _MatrixValue) if m.rows == 1 && m.cols == 4 => Some(m.toVector.map(d => Right(_MatrixValue(1, 1, Array(d)))))
    case other => other.toExpression match
      case row: _Matrix if row.rows == 1 && row.cols == 4 => Some(row.elems.map(_.eval(env)))
      case _                                               => None

/** A model operand of the `control` nodes as a dense matrix — the one reading `dlsim` and the
 *  Riccati family share (F_0064, F_0065): a matrix as itself (an exact one demoted), `0` as the
 *  all-zero block of `zeroShape` when that shape is known, any other number as a `1×1` matrix.
 *
 *  @param r         the evaluated operand
 *  @param zeroShape the shape a bare `0` stands for, when the caller can infer it
 *  @return the dense matrix, or `None` for a non-numeric operand or an unshapeable `0`
 */
private[control] def matrixOperand(r: Either[_Expression, _Value], zeroShape: => Option[(Int, Int)]): Option[_MatrixValue] =
  r match
    case Right(_Number(0.0)) => zeroShape.map((rows, cols) => _MatrixValue(rows, cols, new Array[Double](rows * cols)))
    case Right(_Number(v))   => Some(_MatrixValue(1, 1, Array(v)))
    case other               => _MatrixValue.ofEvaluated(other)


/** `dlsim(sys, u, x0[, states])` — [[dlsim]] as a node (F_0064): the outputs `Y`, or with the
 *  slot word `states` (not reserved, the `svd(A, full)` precedent) the row `[[Y, X]]`, `X`
 *  holding `x[0] … x[N]`. `sys` is the `[[A, B, C, D]]` row `stateSpace`, `linearize`, `arx2ss`
 *  build; `u` is `m×N`, or `N×m` when no side but the other matches; `x0` is a column, or `0` for
 *  rest. A refusal leaves the call as written, and [[detailed]] carries the reason.
 *
 *  @param sys    the model row
 *  @param u      the inputs
 *  @param x0     the initial state
 *  @param states whether to return the states as well */
case class _Dlsim(sys: _Expression, u: _Expression, x0: _Expression, states: Boolean = false) extends _Expression:
  override def children: List[_Expression] = List(sys, u, x0)
  override def rebuild(c: List[_Expression]): _Expression = _Dlsim(c(0), c(1), c(2), states)
  override def toString: String = s"dlsim($sys, $u, $x0${if states then ", states" else ""})"

  /** The simulation, or why it was declined.
   *  @param env the evaluation environment
   *  @return `(Y, X)` or the reason */
  def detailed(env: Environment): Either[SimFailure, (_MatrixValue, _MatrixValue)] =
    modelRow(sys, env) match
      case Some(cells) =>
        for
          a  <- simMatrix(cells(0), "A", None)
          b  <- simMatrix(cells(1), "B", None)
          c  <- simMatrix(cells(2), "C", None)
          d  <- simMatrix(cells(3), "D", Some((c.rows, b.cols)))
          um <- simMatrix(u.eval(env), "u", None)
          ui = if um.rows != b.cols && um.cols == b.cols then um.transpose else um
          x  <- simMatrix(x0.eval(env), "x0", Some((a.rows, 1)))
          xc = if x.rows == 1 && x.cols == a.rows && a.rows > 1 then x.transpose else x
          r  <- dlsim(a, b, c, d, ui, xc)
        yield r
      case _ => Left(NotNumeric("sys is not the [[A, B, C, D]] row stateSpace builds"))

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env) match
      case Left(_)                  => Left(this)
      case Right((y, _)) if !states => Right(y)
      case Right((y, x))            => Left(_Matrix(1, 2, Vector(y, x)))
