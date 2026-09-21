package it.grypho.scala.leonardo
package tools

import org.scalatest.flatspec.AnyFlatSpec

/** The mojibake repair's invariants, asserted rather than described (issue F_0027).
 *
 *  **Every case here existed only as prose in the Python original's docstring.**  Each one is
 *  a rule that, if broken, either stops repairing the most common damage in this repository or
 *  silently corrupts text that was never damaged — and neither shows up as a crash.
 */
class FixMojibakeTest extends AnyFlatSpec:

  /** Damages text the way the original tool did: UTF-8 bytes read as cp1252.
   *
   *  **Not `new String(bytes, "windows-1252")`**, which is the obvious spelling and the wrong
   *  one twice over: Java's decoder maps the five undefined bytes to `U+FFFD`, losing exactly
   *  the 0x81 that `⁻¹` depends on — and a byte-to-char identity map would be Latin-1, which
   *  differs from cp1252 across the whole 0x80–0x9F range where the interesting damage lives.
   *  Inverting the repair's own table is the only spelling that reproduces the original tool,
   *  and the table is verified independently by the two cases above.
   */
  private val ByteToChar: Map[Int, Char] = FixMojibake.CharToByte.map(_.swap)

  private def damage(text: String): String =
    new String(text.getBytes("UTF-8").map(b => ByteToChar.getOrElse(b & 0xFF, (b & 0xFF).toChar)))

  "the cp1252 table" should "carry the five slots the charset leaves undefined" in
  {
    // THE trap of the port: Java's windows-1252 decoder maps these to U+FFFD by default, so a
    // table built without CodingErrorAction.REPORT would map five different bytes onto one
    // character -- and `⁻¹`, whose UTF-8 contains 0x81, would stop being repairable.
    for slot <- List(0x81, 0x8D, 0x8F, 0x90, 0x9D) do
      assert(FixMojibake.CharToByte.get(slot.toChar).contains(slot),
             f"the undefined slot 0x$slot%02X must pass through as itself")
  }

  it should "be injective, so no two bytes decode to the same character" in
  {
    // A table with a duplicate key silently loses a byte, which is what U+FFFD collisions do.
    assert(FixMojibake.CharToByte.values.toVector.distinct.size == FixMojibake.CharToByte.size)
  }

  "the repair" should "recover the sequence that motivated the pass-through" in
  {
    // `⁻¹` is the most common damaged sequence in this codebase and its UTF-8 is E2 81 BB,
    // so it is unrepairable without the undefined-slot handling above.
    assert(FixMojibake.repair(damage("⁻¹")) == "⁻¹")
    assert(FixMojibake.repair(damage("Aᵀ and eˣ and r̂")) == "Aᵀ and eˣ and r̂")
  }

  it should "unwind text that was damaged twice" in
  {
    // The fixpoint, rather than a single pass: an em dash put through the bad decode twice.
    assert(FixMojibake.repair(damage(damage("—"))) == "—")
  }

  it should "leave correct text exactly as it is" in
  {
    // A run that does not round-trip is left alone, which is what makes the tool safe to run
    // over the whole tree: `⁻¹` undamaged is not cp1252-encodable, so its run simply fails.
    for clean <- List("⁻¹", "∫ f dx", "λ → ∂", "r̂", "───", "plain ASCII") do
      assert(FixMojibake.repair(clean) == clean, s"corrupted undamaged text: $clean")
  }

  it should "refuse a round-trip that lands in a script this codebase never uses" in
  {
    // THE non-injectivity case, and the reason the plausibility gate exists: `×–` encodes to
    // bytes D7 96, which is valid UTF-8 for Hebrew ZAYIN. `docs/src/developer.md` contains
    // exactly this text, so without the gate the repair would corrupt the documentation it
    // was run over.
    // The letter is built from its code point, never written: CheckCharset bans it, and a
    // guard whose own fixtures trip a neighbouring guard is a guard nobody can run over the
    // whole tree.
    val zayin = 0x05D6.toChar
    assert(FixMojibake.repair("100×–4 700×") == "100×–4 700×")
    assert(!FixMojibake.repair("100×–4 700×").contains(zayin))
  }

  "the lossy table" should "map each entry it was built for" in
  {
    // These lost a byte outright, so no arithmetic recovers them; each was confirmed against
    // the prose it was found in rather than guessed from the byte pattern.
    for (damaged, original) <- FixMojibake.Lossy do
      assert(FixMojibake.repair(damaged) == original, s"lossy entry not applied: $damaged")
  }

  "a scan of this repository" should "find nothing, which is the state the guard defends" in
  {
    import java.nio.file.Paths
    val roots = List("core/src", "repl/src", "web/src", "tools/src", "docs").map(Paths.get(_))
    val found = FixMojibake.scan(roots)
    assert(found.isEmpty, s"damaged files: ${found.map(_._1).mkString(", ")}")
  }
