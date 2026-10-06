package it.grypho.scala.leonardo
package matrix

import core.*


/** Block matrices (F_0055): `hcat`, `vcat`, `blkdiag`, `repmat`, `kron`, `submatrix` (`ones` sits
 *  beside `zeros` in `_MatrixOperation.scala`, its template).
 *
 *  **One assembly path serves every carrier.**  Each operation reads its evaluated operands as
 *  grids of cells ([[cellGrid]]: a dense value re-inflated through `_Matrix.fromValue`),
 *  rearranges the cells, and evaluates the resulting [[_Matrix]] — which collapses to a dense
 *  `_MatrixValue` when every cell is a number, stays exact when the cells are `_Rational` (an
 *  exact `_Matrix` never collapses, 4.L), and stays symbolic otherwise.  So dense, exact and
 *  symbolic input need no code of their own; `kron` adds only the cell products, with the
 *  dense kernel as its fast path.
 *
 *  **A number is a `1×1` block** where the MATLAB reading has one (`hcat`, `vcat`, `blkdiag`,
 *  `kron`, and `control.mpcMatrices`) — F_0055 Decision C.  **A free name is never guessed**:
 *  it may later be bound to a matrix (issue 1.2), so an operand whose shape is unknown keeps the
 *  node unevaluated until it is bound.
 *
 *  `submatrix` is **1-based and inclusive** (Decision A), the `at(A, i, j)` convention: the
 *  `1×1` slice `submatrix(A, i, i, j, j)` holds exactly `at(A, i, j)`.
 */

/** The most cells a block operation may produce, checked **before** anything is built.
 *
 *  Assembly allocates one node per cell, so the cost is memory and time per cell rather than a
 *  dense array's eight bytes. **Measured** (F_0055, JVM, warm): `repmat([[1]], 1000, 1000)` —
 *  one million cells — assembles and collapses in 126 ms, a million-cell `_Matrix` holding
 *  about 96 MB; the next decade, ten million cells, took 1.9 s and about 937 MB. The
 *  `MaxSampleCount` rule: an uncapped size is an out-of-memory error, not a refusal.
 */
val MaxMatrixCells: Int = 1_000_000

/** An evaluated operand as a grid of cells, or `None` when its shape is not known.
 *
 *  @param r       the evaluated operand
 *  @param scalars whether a real number counts as a `1×1` grid (Decision C)
 *  @return the grid: a matrix as itself, a dense one re-inflated, a number as `1×1` when allowed
 */
private[leonardo] def cellGrid(r: Either[_Expression, _Value], scalars: Boolean): Option[_Matrix] = r match
  case Right(v: _MatrixValue)                 => Some(_Matrix.fromValue(v))
  case Left(m: _Matrix)                       => Some(m)
  case Right(v @ _Number(_)) if scalars       => Some(_Matrix(1, 1, Vector(v)))
  case _                                      => None

/** A positive whole count or index, read from an evaluated operand. */
private[leonardo] def wholeCount(r: Either[_Expression, _Value]): Option[Int] = r match
  case Right(_Number(d)) if d >= 1 && d == math.floor(d) && d <= Int.MaxValue => Some(d.toInt)
  case _                                                                     => None

/** The zero of the tier the given cells are in: exact beside exact cells, so a zero block
 *  cannot demote an exact matrix (the numeric tier rule). */
private[leonardo] def zeroLike(cells: Iterable[_Expression]): _Expression =
  if cells.exists(_.isInstanceOf[_Rational]) then _Rational.Zero else _Number(0)

/** A `rows×cols` block of one repeated cell. */
private[leonardo] def filledBlock(rows: Int, cols: Int, cell: _Expression): _Matrix =
  _Matrix(rows, cols, Vector.fill(rows * cols)(cell))

/** Assembles a grid of blocks into one matrix.
 *
 *  Every block of a block-row must have that row's height and every block of a block-column
 *  that column's width; the result may not exceed [[MaxMatrixCells]], which is checked before
 *  any cell is copied.
 *
 *  @param grid the block rows, each a non-empty row of blocks, all of the same length
 *  @return the assembled matrix, or `None` when the blocks do not conform or it is too large
 */
private[leonardo] def assembleBlocks(grid: Vector[Vector[_Matrix]]): Option[_Matrix] =
  if grid.isEmpty || grid.exists(_.isEmpty) then None
  else
    val heights  = grid.map(_.head.rows)
    val widths   = grid.head.map(_.cols)
    val conforms = grid.forall(_.size == widths.size) && grid.zip(heights).forall { (row, h) =>
      row.zip(widths).forall((b, w) => b.rows == h && b.cols == w)
    }
    Option.when(conforms && heights.sum.toLong * widths.sum <= MaxMatrixCells) {
      val cells = for (row, h) <- grid.zip(heights); i <- 0 until h; b <- row; j <- 0 until b.cols yield b(i, j)
      _Matrix(heights.sum, widths.sum, cells)
    }

/** Evaluates the operands, reads them as grids, builds the result; any unknown shape or a
 *  failed `build` leaves the node unevaluated with its operands reduced. */
private def blockEval(operands: List[_Expression], env: Environment, scalars: Boolean,
                      unevaluated: List[_Expression] => _Expression)
                     (build: Vector[_Matrix] => Option[_Matrix]): Either[_Expression, _Value] =
  val reduced = operands.map(_.eval(env))
  val grids   = reduced.map(cellGrid(_, scalars))
  (if grids.forall(_.isDefined) then build(grids.flatten.toVector) else None) match
    case Some(m) => m.eval(env)
    case None    => Left(unevaluated(reduced.map(_.toExpression)))


/** Horizontal concatenation: `hcat(A, B, …)`, blocks of equal height side by side.
 *
 *  @param blocks the blocks, left to right; a number is a `1×1` block
 */
case class _HCat(blocks: List[_Expression]) extends _MatrixOperation:
  override def toString: String = blocks.mkString("hcat(", ", ", ")")
  override def children: List[_Expression] = blocks
  override def rebuild(c: List[_Expression]): _Expression = _HCat(c)
  override def eval(env: Environment): Either[_Expression, _Value] =
    blockEval(blocks, env, scalars = true, _HCat(_))(gs => assembleBlocks(Vector(gs)))

/** Vertical concatenation: `vcat(A, B, …)`, blocks of equal width stacked.
 *
 *  @param blocks the blocks, top to bottom; a number is a `1×1` block
 */
case class _VCat(blocks: List[_Expression]) extends _MatrixOperation:
  override def toString: String = blocks.mkString("vcat(", ", ", ")")
  override def children: List[_Expression] = blocks
  override def rebuild(c: List[_Expression]): _Expression = _VCat(c)
  override def eval(env: Environment): Either[_Expression, _Value] =
    blockEval(blocks, env, scalars = true, _VCat(_))(gs => assembleBlocks(gs.map(Vector(_))))

/** Block diagonal: `blkdiag(A, B, …)`, the blocks on the diagonal and zeros elsewhere — zeros
 *  in the blocks' tier, so exact blocks give an exact matrix.
 *
 *  @param blocks the diagonal blocks, top-left first; a number is a `1×1` block
 */
case class _BlkDiag(blocks: List[_Expression]) extends _MatrixOperation:
  override def toString: String = blocks.mkString("blkdiag(", ", ", ")")
  override def children: List[_Expression] = blocks
  override def rebuild(c: List[_Expression]): _Expression = _BlkDiag(c)
  override def eval(env: Environment): Either[_Expression, _Value] =
    blockEval(blocks, env, scalars = true, _BlkDiag(_)) { gs =>
      // The zero blocks cost cells too, so the size is checked before any is built.
      if gs.map(_.rows).sum.toLong * gs.map(_.cols).sum > MaxMatrixCells then None
      else
        val zero = zeroLike(gs.flatMap(_.elems))
        assembleBlocks(gs.indices.toVector.map { i =>
          gs.indices.toVector.map(j => if i == j then gs(i) else filledBlock(gs(i).rows, gs(j).cols, zero))
        })
    }

/** Tiling: `repmat(A, m, n)`, `A` repeated `m` times down and `n` times across.
 *
 *  @param m    the block to tile (a matrix: Decision C does not extend to `repmat`)
 *  @param down the number of copies down, a positive integer
 *  @param across the number of copies across, a positive integer
 */
case class _RepMat(m: _Expression, down: _Expression, across: _Expression) extends _MatrixOperation:
  override def toString: String = s"repmat($m, $down, $across)"
  override def children: List[_Expression] = List(m, down, across)
  override def rebuild(c: List[_Expression]): _Expression = _RepMat(c.head, c(1), c(2))
  override def eval(env: Environment): Either[_Expression, _Value] =
    val reduced = children.map(_.eval(env))
    val built = for
      g <- cellGrid(reduced.head, scalars = false)
      r <- wholeCount(reduced(1))
      c <- wholeCount(reduced(2))
      if g.rows.toLong * r * g.cols * c <= MaxMatrixCells
      out <- assembleBlocks(Vector.fill(r)(Vector.fill(c)(g)))
    yield out
    built.map(_.eval(env)).getOrElse(Left(rebuild(reduced.map(_.toExpression))))

/** The [[https://en.wikipedia.org/wiki/Kronecker_product Kronecker product]]: `kron(A, B)`.
 *
 *  Dense operands use [[core._MatrixValue.kronecker]]; otherwise each cell is the product of a
 *  cell of `A` and a cell of `B`, through `productOf`, which keeps an exact zero exact.
 *
 *  @param a the left factor; a number is a `1×1` matrix
 *  @param b the right factor; a number is a `1×1` matrix
 */
case class _Kron(a: _Expression, b: _Expression) extends _MatrixOperation:
  override def toString: String = s"kron($a, $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = _Kron(c.head, c(1))
  override def eval(env: Environment): Either[_Expression, _Value] =
    val (ra, rb) = (a.eval(env), b.eval(env))
    lazy val unevaluated = Left(_Kron(ra.toExpression, rb.toExpression))
    (cellGrid(ra, scalars = true), cellGrid(rb, scalars = true)) match
      case (Some(x), Some(y)) if x.rows.toLong * y.rows * x.cols * y.cols <= MaxMatrixCells =>
        (ra, rb) match
          case (Right(dx: _MatrixValue), Right(dy: _MatrixValue)) => dx.kronecker(dy).guarded(this)
          case _ =>
            val (rows, cols) = (x.rows * y.rows, x.cols * y.cols)
            _Matrix(rows, cols, Vector.tabulate(rows * cols) { k =>
              val (i, j) = (k / cols, k % cols)
              productOf(x(i / y.rows, j / y.cols), y(i % y.rows, j % y.cols))
            }).eval(env)
      case _ => unevaluated

/** A slice: `submatrix(A, r0, r1, c0, c1)`, rows `r0..r1` and columns `c0..c1`, **1-based and
 *  inclusive** (Decision A).  Out-of-range, reversed or non-integer bounds leave it unevaluated.
 *
 *  @param m  the matrix to slice
 *  @param r0 the first row, from 1
 *  @param r1 the last row, inclusive
 *  @param c0 the first column, from 1
 *  @param c1 the last column, inclusive
 */
case class _SubMatrix(m: _Expression, r0: _Expression, r1: _Expression, c0: _Expression, c1: _Expression)
    extends _MatrixOperation:
  override def toString: String = s"submatrix($m, $r0, $r1, $c0, $c1)"
  override def children: List[_Expression] = List(m, r0, r1, c0, c1)
  override def rebuild(c: List[_Expression]): _Expression = _SubMatrix(c.head, c(1), c(2), c(3), c(4))
  override def eval(env: Environment): Either[_Expression, _Value] =
    val reduced = children.map(_.eval(env))
    val built = for
      g    <- cellGrid(reduced.head, scalars = false)
      top  <- wholeCount(reduced(1))
      bot  <- wholeCount(reduced(2))
      left <- wholeCount(reduced(3))
      rgt  <- wholeCount(reduced(4))
      if top <= bot && bot <= g.rows && left <= rgt && rgt <= g.cols
    yield _Matrix(bot - top + 1, rgt - left + 1,
                  for i <- (top - 1 until bot).toVector; j <- left - 1 until rgt yield g(i, j))
    built.map(_.eval(env)).getOrElse(Left(rebuild(reduced.map(_.toExpression))))
