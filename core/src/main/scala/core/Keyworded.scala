package it.grypho.scala.leonardo
package core


/** A kind whose cases are spelled by a word at the prompt — a distribution family, a test, a
 *  method slot word (F_0148).
 *
 *  Seventeen kind enums spelled that word three ways — a `val word` on the case, a companion
 *  `keyword(k)`, a `fnName` — and only three offered the reverse lookup.  Each now mixes this in
 *  with a concrete `keyword`, so one question ("what does the user type for this?") has one
 *  spelling, and [[Keyworded.byKeyword]] answers the reverse for all of them.  A trait, not an
 *  abstract member on a shared base: the enums are sealed, and mixing a trait in adds an interface
 *  rather than an obligation (the MiMa rule `NamedFunction` follows).
 */
trait Keyworded:
  /** The word the grammar reads for this case.
   *  @return the keyword */
  def keyword: String

/** The reverse lookup every [[Keyworded]] kind shares. */
object Keyworded:
  /** The case spelled `word`, if any.
   *  @tparam A    the kind
   *  @param values the kind's cases
   *  @param word   the keyword
   *  @return the case, or `None` */
  def byKeyword[A <: Keyworded](values: Iterable[A], word: String): Option[A] = values.find(_.keyword == word)
