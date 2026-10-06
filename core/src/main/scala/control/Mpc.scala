package it.grypho.scala.leonardo
package control

import core.*
import matrix.*


/** The MPC prediction matrices (F_0055, MPC requirement T1-03).
 *
 *  For `x(k+1) = A·x(k) + B·u(k)`, `y = C·x`, the stacked outputs over the prediction horizon
 *  `Np` are `Y = Φ·x(k) + Γ·U` with `U` the `Nc` future moves:
 *
 *  - `Φ` stacks `C·Aⁱ` for `i = 1..Np`;
 *  - `Γ`'s block `(i, j)` is `C·Aⁱ⁻ʲ·B` for `j ≤ i < …`, zero above the diagonal, and its **last
 *    column accumulates** `Σ C·Aᵏ·B` for `k = 0..i−Nc` — the last move held over the rest of
 *    the horizon, stated because the other common convention (moves beyond `Nc` are zero) gives
 *    a different `Γ` for the same plant.
 *
 *  Built from ordinary matrix products and sums over the block-matrix cell grids, so it is
 *  exact on exact input and symbolic on symbolic input, like every other block operation.
 */

/** The longest prediction horizon accepted.
 *
 *  `Np` matrix products and `Np` running sums are computed, each over re-inflated cell grids.
 *  **Measured** (F_0055, JVM, warm): a ten-state single-input plant takes 44 ms at `Np = 100`
 *  and 464 ms at `Np = 1000`, `Nc = 10`; MPC horizons are tens to hundreds of steps, so the cap
 *  refuses only a mistyped digit.
 */
val MaxMpcHorizon: Int = 1000

/** The prediction matrices `(Φ, Γ)` of `(A, B, C)` over the horizons `Np` and `Nc`.
 *
 *  A number is read as a `1×1` matrix (F_0055 Decision C); an operand whose shape is not known
 *  (a free name) declines.
 *
 *  @param a   the state matrix, `n×n`
 *  @param b   the input matrix, `n×m`
 *  @param c   the output matrix, `p×n`
 *  @param np  the prediction horizon, `1 ≤ Np ≤ MaxMpcHorizon`
 *  @param nc  the control horizon, `1 ≤ Nc ≤ Np`
 *  @param env the environment the operands evaluate in
 *  @return `(Φ, Γ)` as evaluated matrices, or `None` when the plant does not conform, a horizon
 *          is out of range, an operand's shape is unknown, or a result would exceed
 *          `matrix.MaxMatrixCells`
 */
def mpcMatrices(a: _Expression, b: _Expression, c: _Expression, np: Int, nc: Int,
                env: Environment): Option[(_Expression, _Expression)] =
  def grid(e: _Expression): Option[_Matrix] = cellGrid(e.eval(env), scalars = true)
  def times(x: _Matrix, y: _Matrix): Option[_Matrix] = cellGrid(MatProduct(x, y).eval(env), scalars = true)
  def plus(x: _Matrix, y: _Matrix): Option[_Matrix]  = cellGrid(MatSum(x, y).eval(env), scalars = true)
  /** Folds `step` over `1..count`, starting from `first`, stopping at the first failure. */
  def iterate(first: _Matrix, count: Int)(step: _Matrix => Option[_Matrix]): Option[Vector[_Matrix]] =
    (1 until count).foldLeft(Option(Vector(first)))((acc, _) => acc.flatMap(v => step(v.last).map(v :+ _)))

  for
    am <- grid(a); bm <- grid(b); cm <- grid(c)
    n   = am.rows
    (m, p) = (bm.cols, cm.rows)
    if am.cols == n && bm.rows == n && cm.cols == n
    if 1 <= nc && nc <= np && np <= MaxMpcHorizon
    if np.toLong * p * math.max(n, nc * m) <= MaxMatrixCells
    zero    = zeroLike(am.elems ++ bm.elems ++ cm.elems)
    caPow  <- iterate(cm, np + 1)(times(_, am))                   // C·Aᵏ, k = 0..Np
    markov <- caPow.take(np).foldLeft(Option(Vector.empty[_Matrix]))((acc, x) => acc.flatMap(v => times(x, bm).map(v :+ _)))
    // held(k) = Σ C·Aʲ·B for j = 0..k: the last move's accumulated effect, k steps after it starts.
    held   <- markov.tail.foldLeft(Option(Vector(markov.head)))((acc, h) => acc.flatMap(v => plus(v.last, h).map(v :+ _)))
    phi    <- assembleBlocks(caPow.tail.map(Vector(_)))
    gamma  <- assembleBlocks(Vector.tabulate(np, nc) { (i, j) =>
                if i < j then filledBlock(p, m, zero)
                else if j < nc - 1 then markov(i - j)
                else held(i - j)
              })
  yield (phi.eval(env).toExpression, gamma.eval(env).toExpression)

/** `mpcMatrices(A, B, C, Np, Nc)` — the prediction matrices as the row `[[Phi, Gamma]]`, the
 *  `lu`/`stateSpace` shape, so `Phi, Gamma := mpcMatrices(…)` binds both (Decision D).
 *
 *  @param a  the state matrix
 *  @param b  the input matrix
 *  @param c  the output matrix
 *  @param np the prediction horizon
 *  @param nc the control horizon
 */
case class _MpcMatrices(a: _Expression, b: _Expression, c: _Expression, np: _Expression, nc: _Expression)
    extends _Expression:
  override def toString: String = s"mpcMatrices($a, $b, $c, $np, $nc)"
  override def children: List[_Expression] = List(a, b, c, np, nc)
  override def rebuild(cs: List[_Expression]): _Expression = _MpcMatrices(cs.head, cs(1), cs(2), cs(3), cs(4))
  override def eval(env: Environment): Either[_Expression, _Value] =
    val reduced = children.map(_.eval(env))
    val built = for
      p <- wholeCount(reduced(3))
      k <- wholeCount(reduced(4))
      (phi, gamma) <- mpcMatrices(reduced.head.toExpression, reduced(1).toExpression, reduced(2).toExpression, p, k, env)
    yield _Matrix(1, 2, Vector(phi, gamma))
    built.map(Left(_)).getOrElse(Left(rebuild(reduced.map(_.toExpression))))
