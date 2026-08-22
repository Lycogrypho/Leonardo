package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec


/** Covers the key-chord decisions of issue 4.K.
 *
 *  Deliberately builds **no** JLine `Terminal`.  An earlier version of this suite did, to
 *  inspect a live key map, and that intermittently crashed the JVM inside sbt's in-process
 *  runner with an EXCEPTION_ACCESS_VIOLATION during native library load (JLine extracts and
 *  loads a native DLL per terminal).  A control run of three full builds with this file
 *  removed was clean, so the terminal was the trigger.
 *
 *  Nothing is lost: the invariant worth guarding — that no chord steals an editing
 *  shortcut — is a property of the binding list, not of a live terminal, so asserting it
 *  here is both deterministic and sharper.
 */
class GreekChordTest extends AnyFlatSpec:

  val Esc: String = 27.toChar.toString

  "the chord list" should "bind the ALT-\\ prefix for every listed symbol" in
  {
    val sequences = greekKeySequences.toMap
    for (letter, symbol) <- GreekChords do
      val seq = s"$Esc\\$letter"
      assert(sequences.contains(seq), s"ALT-\\ $letter should be bound (inserts $symbol)")
      assert(sequences(seq) == s"insert-greek-$letter", s"wrong widget for ALT-\\ $letter")
  }

  it should "also bind ALT-G directly, which the keymap dump showed is free" in
  {
    assert(greekKeySequences.toMap.get(s"${Esc}g").contains("insert-greek-g"))
  }

  it should "never collide with a shortcut the emacs key map already owns" in
  {
    // the entire reason a PREFIX was chosen over one chord per symbol
    for (seq, widget) <- greekKeySequences do
      for (letter, action) <- EmacsReservedChords do
        assert(seq != s"$Esc$letter",
          s"$widget would steal ALT-$letter ($action)")
  }

  it should "in particular leave ALT-b alone" in
  {
    assert(EmacsReservedChords.get("b").contains("backward-word"), "precondition")
    assert(!greekKeySequences.exists((seq, _) => seq == s"${Esc}b"),
      "ALT-b is backward-word and must not be rebound")
    // the capital is not an escape hatch: it resolves to ALT-b via do-lowercase-version
    assert(!greekKeySequences.exists((seq, _) => seq == s"${Esc}B"),
      "ALT-B redirects to ALT-b, so binding it takes backward-word just the same")
  }

  it should "bind each key sequence exactly once" in
  {
    // Duplicate SEQUENCES would be a bug -- the later bind silently overrides the earlier.
    val seqs = greekKeySequences.map(_._1)
    assert(seqs.distinct.size == seqs.size, s"duplicate key sequences: $seqs")
    // Duplicate WIDGETS are intended: ALT-\ g and ALT-G both insert the same symbol, so
    // one insertion action is deliberately reachable two ways.
    assert(greekKeySequences.count(_._2 == "insert-greek-g") == 2,
      "gamma should be reachable both from the prefix and from ALT-G")
  }

  "GreekChords" should "only offer symbols the grammar can actually read" in
  {
    // inserting a character the parser rejects would just manufacture parse errors
    for (_, symbol) <- GreekChords do
      val src = if symbol == "β" then s"$symbol(1, 2)" else s"$symbol(3)"
      assert(parser.Parser.parse(src).successful, s"the grammar must accept $src")
  }

  it should "alias exactly the two special functions, and not capital Beta" in
  {
    assert(GreekChords.map(_._2).toSet == Set("Γ", "β"))
    // U+0392 is a homoglyph of Latin B and is deliberately absent
    assert(!GreekChords.exists((_, s) => s == "Β"), "capital Greek Beta must not be offered")
  }