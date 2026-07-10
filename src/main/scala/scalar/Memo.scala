package it.grypho.scala.leonardo
package scalar


// Bounded, thread-safe memo table for pure algorithm results (derive, simplify).
// Memoization is semantically transparent: the cached functions depend only on the
// structure of their arguments, never on an Environment, so a hit and a recompute
// are indistinguishable. Expressions are immutable and hash/compare structurally,
// which makes them sound map keys.
//
// Eviction is deliberately crude — clear everything when the bound is reached —
// because correctness never depends on retention; the bound only caps memory.
// This is the pragmatic core of the legacy hash-consing idea: repeated work on
// shared subtrees is paid once, without interning's global identity table.
private[scalar] final class Memo[K, V <: AnyRef](maxEntries: Int):
  private val table = new java.util.concurrent.ConcurrentHashMap[K, V]()

  // The nullable ConcurrentHashMap results are wrapped in Option at the boundary,
  // so null never escapes into the Scala code (see CLAUDE.md code style).
  def getOrElseUpdate(key: K)(compute: => V): V =
    Option(table.get(key)).getOrElse {
      if table.size >= maxEntries then table.clear()
      val computed = compute
      Option(table.putIfAbsent(key, computed)).getOrElse(computed)
    }
