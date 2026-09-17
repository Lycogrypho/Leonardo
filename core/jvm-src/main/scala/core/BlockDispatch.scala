package it.grypho.scala.leonardo
package core

import java.util.stream.IntStream

/** How a block-partitioned kernel is dispatched — **the JVM implementation** (issue F_0003).
 *
 *  This is the whole of the library's platform-specific surface.  A sweep of `core/src/main`
 *  for `java.*`, threading, parallel collections, futures and file IO finds exactly three
 *  uses; `ConcurrentHashMap` and `ThreadLocal` are both in Scala.js's javalib, leaving
 *  `java.util.stream`, which is not, as the only thing that had to move.
 *
 *  Keeping it behind one object rather than inlining a platform check means the shared source
 *  never mentions a platform at all, and a future Scala Native target adds a third file rather
 *  than a third branch.
 */
private[core] object BlockDispatch:

  /** Runs `kernel` for every block index in `0 until blocks`.
   *
   *  Parallel above the caller's work threshold: the blocks write disjoint slices of the
   *  output, so there is no locking and no accumulation order to preserve *between* blocks.
   *  Within a block the kernel accumulates `k` ascending, which is what keeps the tiled result
   *  bit-identical to the untiled one — and therefore identical on both platforms.
   *
   *  @param blocks   number of blocks to cover
   *  @param parallel whether the caller's work-volume threshold was met
   *  @param kernel   the per-block body, called once per index
   */
  inline def foreachBlock(blocks: Int, parallel: Boolean)(kernel: Int => Unit): Unit =
    if parallel then IntStream.range(0, blocks).parallel().forEach(b => kernel(b))
    else
      var b = 0
      while b < blocks do
        kernel(b)
        b += 1
