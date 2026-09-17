package it.grypho.scala.leonardo
package core

/** How a block-partitioned kernel is dispatched — **the Scala.js implementation** (F_0003).
 *
 *  Always sequential, and that is **correct rather than a degradation**: JavaScript is
 *  single-threaded, and the shared-memory parallelism this would need (`SharedArrayBuffer`
 *  with cross-origin isolation) is not available to an ordinary page and would not fit a
 *  `forEach` shape anyway.
 *
 *  The `parallel` flag is accepted and ignored so the shared call site stays identical on both
 *  platforms.  Results are unaffected: the JVM's parallel branch already had to produce the
 *  same values as its sequential one, because blocks write disjoint output slices.
 */
private[core] object BlockDispatch:

  /** Runs `kernel` for every block index in `0 until blocks`, sequentially.
   *
   *  @param blocks   number of blocks to cover
   *  @param parallel ignored on this platform; see the class comment
   *  @param kernel   the per-block body, called once per index
   */
  inline def foreachBlock(blocks: Int, parallel: Boolean)(kernel: Int => Unit): Unit =
    var b = 0
    while b < blocks do
      kernel(b)
      b += 1
