package it.grypho.scala.leonardo
package scalar


/** Bounded, thread-safe memoisation table for pure algorithm results (derive, simplify).
 *
 *  Memoisation is semantically transparent: the cached functions depend only on the
 *  structure of their arguments, never on an [[core.Environment]], so a cache hit and a
 *  recompute are indistinguishable.  Expressions are immutable and hash/compare
 *  structurally, which makes them sound map keys.
 *
 *  Eviction is deliberately crude — clear everything when the bound is reached — because
 *  correctness never depends on retention; the bound only caps memory.  This is the
 *  pragmatic core of the legacy hash-consing idea: repeated work on shared subtrees is
 *  paid once, without interning's global identity table.
 *
 *  @tparam K key type (expression or `(expression, variable)` pair)
 *  @tparam V value type; `AnyRef` so the underlying `ConcurrentHashMap` can store it without boxing
 *  @param maxEntries eviction threshold; the table is cleared when size reaches this value
 */
private[scalar] final class Memo[K, V <: AnyRef](maxEntries: Int):
  private val table = new java.util.concurrent.ConcurrentHashMap[K, V]()

  /** Returns the cached result for `key`, or computes and caches it if absent.
   *
   *  The nullable `ConcurrentHashMap` result is wrapped in `Option` at the boundary so
   *  `null` never escapes into Scala code.
   *
   *  @param key     the cache key
   *  @param compute the computation to run on a cache miss; called at most once per key
   *  @return the cached or freshly computed value
   */
  def getOrElseUpdate(key: K)(compute: => V): V =
    Option(table.get(key)).getOrElse {
      if table.size >= maxEntries then table.clear()
      val computed = compute
      Option(table.putIfAbsent(key, computed)).getOrElse(computed)
    }
