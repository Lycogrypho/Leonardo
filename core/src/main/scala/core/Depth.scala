package it.grypho.scala.leonardo
package core


/** The deepest expression the library accepts or builds (F_0078): parsed input deeper than this
 *  is refused, and a symbolic `sum`/`product` or an `expand` that would build one declines.
 *
 *  **What overflows is tree depth, and a long sum IS a deep tree** — `x1 + … + xN` is an
 *  `N`-deep left spine, so the recursive walkers descend `N` frames.  `Parser.MaxDepth` counts
 *  nesting only, so it never saw a flat chain.  **The shallowest platform decides** (the
 *  `Parser.MaxDepth` rule): measured on Node with fresh variables at every size, `simplify`
 *  survives a 250-deep chain and overflows at 300, `derive` at 350; the JVM's default stack
 *  takes `simplify` to 600 and fails at 800 — with a `StackOverflowError` that ends the REPL
 *  session.  Eval, `toString`, `expand` and the LaTeX renderer all survive 1000.  **Measure
 *  with fresh names**: `simplify` and `derive` are memoised, and a chain that extends a shorter
 *  one already walked hits the cache part-way down, which is how an earlier figure of "500
 *  survives" came about.  F_0086 (walkers that do not recurse down a spine) is what would let
 *  this rise.
 */
val MaxExpressionDepth: Int = 200

/** The depth of `e` — a leaf is depth 1 — counted no further than `limit + 1`, iteratively,
 *  so a deep tree cannot overflow the very check that exists to refuse it.
 *
 *  @param e     the expression
 *  @param limit the depth past which counting stops
 *  @return the depth, or `limit + 1` when it exceeds `limit`
 */
private[leonardo] def depthUpTo(e: _Expression, limit: Int): Int =
  @annotation.tailrec
  def go(pending: List[(_Expression, Int)], deepest: Int): Int =
    if deepest > limit then deepest
    else pending match
      case Nil                => deepest
      case (node, d) :: rest  => go(node.children.map(c => (c, d + 1)) ::: rest, math.max(deepest, d))
  go(List((e, 1)), 0)

/** Whether `e` is deeper than [[MaxExpressionDepth]].
 *
 *  @param e the expression
 *  @return `true` when it is too deep for every platform's walkers
 */
private[leonardo] def tooDeep(e: _Expression): Boolean = depthUpTo(e, MaxExpressionDepth) > MaxExpressionDepth

/** The number of nodes of `e` as a tree — a shared subterm counted at every occurrence, since
 *  that is what a walker pays — counted no further than `limit + 1`, iteratively (F_0183).
 *
 *  @param e     the expression
 *  @param limit the count past which counting stops
 *  @return the node count, or `limit + 1` when it exceeds `limit`
 */
private[leonardo] def nodesUpTo(e: _Expression, limit: Int): Int =
  @annotation.tailrec
  def go(pending: List[_Expression], seen: Int): Int =
    if seen > limit then seen
    else pending match
      case Nil          => seen
      case node :: rest => go(node.children ::: rest, seen + 1)
  go(List(e), 0)
