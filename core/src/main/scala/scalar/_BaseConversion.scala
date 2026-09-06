package it.grypho.scala.leonardo
package scalar

import core.*


/** `tobase(n, b)` — reads `n` as an integer written in base `b` — issue 3.5.
 *
 *  Evaluates to a `core._Based`, which *is* the number and additionally remembers its
 *  base for display.  There is no inverse node: a `_Based` already reads as its value
 *  through `_Number`'s widening extractor, so `tobase(255, 16) + 0` is `255`.
 *
 *  Stays symbolic when `n` is not a whole number or `b` is not an integer in
 *  `_Based.MinBase .. _Based.MaxBase` — the smart factory would collapse those to a plain
 *  `_Number`, which would silently discard the request rather than refuse it.
 *
 *  @param e    the value to re-base
 *  @param base the radix
 */
case class _ToBase(e: _Expression, base: _Expression) extends _Expression:
  override def toString: String = s"tobase($e, $base)"
  override def children: List[_Expression] = List(e, base)
  override def rebuild(c: List[_Expression]): _Expression = _ToBase(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (e.eval(env), base.eval(env)) match
      case (Right(_Number(v)), Right(_Number(b)))
          if v.isWhole && b.isWhole && b >= _Based.MinBase && b <= _Based.MaxBase =>
        Right(_Based.of(v, b.toInt))
      case _ => Left(this)


/** `balanced(n)` — the balanced-ternary form of `n`, with digits `{-1, 0, 1}`.
 *
 *  Written `T`/`0`/`1`, the same alphabet 4.G's symmetric ternary logic uses for
 *  `{false, unknown, true}`.  That shared alphabet is why balanced ternary belongs in this
 *  library rather than being a bare utility: `logic symmetric on` and `balanced(n)` are the
 *  same three digits seen from two sides.
 *
 *  @param e the value to re-base
 */
case class _Balanced(e: _Expression) extends _Expression:
  override def toString: String = s"balanced($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _Balanced(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(v)) if v.isWhole => Right(_Based.of(v, 3, balanced = true))
      case _                              => Left(this)
