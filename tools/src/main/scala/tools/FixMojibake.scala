package it.grypho.scala.leonardo
package tools

import java.nio.ByteBuffer
import java.nio.charset.{CharsetDecoder, CodingErrorAction, StandardCharsets, Charset}
import java.nio.file.{Path, Paths}
import scala.util.Try

/** Repairs UTF-8-read-as-cp1252 mojibake (issue 2.8).
 *
 *  The damage is `original.getBytes(UTF_8)` decoded as cp1252, so the repair is that round
 *  trip inverted.  Three complications make a naive re-encode insufficient, and each is
 *  handled explicitly below.
 *
 *  1. **cp1252 leaves five slots undefined** (0x81, 0x8D, 0x8F, 0x90, 0x9D).  The tool that
 *     did the original damage passed those bytes through as raw C1 code points, so the single
 *     most common sequence here — `⁻¹`, whose UTF-8 is E2 **81** BB C2 B9 — depends on them.
 *     [[CharToByte]] therefore maps U+0081 and its four neighbours straight back to their byte
 *     value.
 *  1. **Some text was damaged twice.**  `Ã¢â‚¬â€` is an em dash that went through the bad decode
 *     two times over, so [[repair]] iterates to a fixpoint rather than passing once.
 *  1. **Some damage is lossy and cannot be reversed arithmetically.**  Where a later tool
 *     stripped a C1 control outright, or normalised the smart quote U+201D to an ASCII `"`,
 *     the byte is simply gone.  [[Lossy]] maps those few sequences by hand; every entry was
 *     confirmed against the surrounding prose, not guessed from the byte pattern.
 *
 *  Correct text is safe because the repair works on maximal non-ASCII runs and substitutes
 *  only when the whole run round-trips: a genuine `⁻¹` (U+207B) is not cp1252-encodable, so
 *  its run fails and is left exactly as it was.
 *
 *  **Ported from Python by F_0027, and the JVM differs exactly where this is subtle.**  Java's
 *  `windows-1252` decoder maps the five undefined bytes to `U+FFFD` by default, where Python's
 *  codec raises — so building the table naively would map five *different* bytes onto one
 *  character and silently lose the `⁻¹` case.  [[CharToByte]] is built with
 *  `CodingErrorAction.REPORT` for that reason, and `MojibakeTest` pins it.
 */
object FixMojibake:

  /** The five slots cp1252 leaves undefined, which the damaging tool emitted as C1 controls. */
  private val UndefinedSlots = Vector(0x81, 0x8D, 0x8F, 0x90, 0x9D)

  /** cp1252 byte → character, inverted; built from the charset so it cannot drift from reality.
   *
   *  `REPORT` rather than the default `REPLACE`: the undefined slots must *fail* to decode so
   *  they can be recognised and added deliberately, instead of all five arriving as `U+FFFD`.
   */
  val CharToByte: Map[Char, Int] =
    val decoder: CharsetDecoder = Charset.forName("windows-1252").newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    val decoded = (0x80 to 0xFF).flatMap { b =>
      Try(decoder.reset().decode(ByteBuffer.wrap(Array(b.toByte))).toString)
        .toOption.filter(_.length == 1).map(_.charAt(0) -> b)
    }
    (decoded ++ UndefinedSlots.map(b => b.toChar -> b)).toMap

  /** Damage that lost a byte outright and so cannot be inverted; each confirmed against context.
   *
   *  **The damaged side is built from code points, never written literally**, and that is not
   *  fastidiousness: this file is a `.scala` under a tree [[CheckCharset]] scans, so a literal
   *  `â‹euro›…` would make the mojibake guard report its own source as damaged.  The Python
   *  original could write them plainly only because `.py` was not a scanned extension — an
   *  accident the port had to replace with a decision.  Each sequence is spelled beside its
   *  bytes, which is what a reader needs anyway.
   */
  val Lossy: Vector[(String, String)] = Vector(
    // E2 80 [94] -> em dash; the 0x94 smart quote was later normalised to an ASCII quote.
    chars(0x00E2, 0x20AC, 0x0022) -> "—",
    // E2 [94] 80 -> box-drawing horizontal, used in this codebase's separator comments.
    chars(0x00E2, 0x0022, 0x20AC) -> "─",
    // E2 [81] B4 / B5 -> superscript four / five; the C1 control was deleted outright.
    chars(0x00E2, 0x00B4) -> "⁴",
    chars(0x00E2, 0x00B5) -> "⁵"
  )

  /** A string from its code points, so damaged text can be named without being written. */
  private def chars(codePoints: Int*): String = codePoints.map(_.toChar).mkString

  /** Scripts this project never uses — read from [[CheckCharset.Forbidden]] so the two cannot
   *  drift, **minus the C1 controls**.
   *
   *  Excluding C1 is not an oversight: the five undefined cp1252 slots *are* C1 code points,
   *  so the intermediate text of a doubly-damaged run legitimately contains them.  Rejecting
   *  them here would refuse the very repairs the pass-through exists to make.
   */
  private val Forbidden: Vector[(Int, Int)] =
    CheckCharset.Forbidden.collect { case (lo, hi, name) if name != "C1 control" => (lo, hi) }

  /** Whether a round-trip's output stays in a script this codebase can contain.
   *
   *  **The round trip is not injective over correct text**: `×–` (U+00D7 U+2013) encodes to
   *  bytes D7 96, which is valid UTF-8 for Hebrew ZAYIN (U+05D6) — so a genuine "100×–4 700×"
   *  in `docs/src/developer.md` would silently become "100‹zayin›4 700×".  (The letter itself is
   *  named rather than written here, because [[CheckCharset]] bans it and these guards must be
   *  clean of their own rules.)  Real mojibake here
   *  always decodes back to Latin, Greek, punctuation or mathematical symbols, so a result
   *  outside those is evidence the input was never damaged in the first place.
   */
  private def plausible(decoded: String): Boolean =
    !decoded.exists(c => Forbidden.exists((lo, hi) => c >= lo && c <= hi))

  /** The cp1252 encoding the damaging tool's decode implies, or `None` if this run is clean. */
  private def toBytes(run: String): Option[Array[Byte]] =
    val bytes = run.map(CharToByte.get)
    Option.when(bytes.forall(_.isDefined))(bytes.flatten.map(_.toByte).toArray)

  /** Strict UTF-8, so an invalid sequence is a refusal rather than a row of `U+FFFD`. */
  private def decodeUtf8(bytes: Array[Byte]): Option[String] =
    val decoder = StandardCharsets.UTF_8.newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    Try(decoder.decode(ByteBuffer.wrap(bytes)).toString).toOption

  /** One repair pass over maximal non-ASCII runs. */
  private def onePass(text: String): String =
    val out = new StringBuilder
    val run = new StringBuilder

    def flush(): Unit =
      if run.nonEmpty then
        val damaged = run.toString
        run.clear()
        val repaired = toBytes(damaged).flatMap(decodeUtf8)
          .filter(d => d != damaged && plausible(d))
        out ++= repaired.getOrElse(damaged)

    text.foreach { ch =>
      if ch >= 0x80 then run += ch
      else
        flush()
        out += ch
    }
    flush()
    out.toString

  /** How many times a doubly-damaged run may be unwound before giving up.
   *
   *  Two is what the known damage needs; five leaves room and still terminates on input that
   *  somehow keeps round-tripping.
   */
  private val MaxPasses = 5

  /** Applies the lossy table, then iterates the round trip to a fixpoint.
   *
   *  @param text possibly damaged text
   *  @return the repaired text, or `text` unchanged when nothing round-trips
   */
  def repair(text: String): String =
    @annotation.tailrec
    def fixpoint(current: String, remaining: Int): String =
      val next = onePass(current)
      if next == current || remaining <= 1 then next else fixpoint(next, remaining - 1)

    fixpoint(Lossy.foldLeft(text)((acc, pair) => acc.replace(pair._1, pair._2)), MaxPasses)

  /** Every file under `roots` this would change, with its before and after. */
  def scan(roots: Seq[Path]): Vector[(Path, String, String)] =
    roots.toVector.flatMap(Files.walk(_, Files.TextExtensions)).flatMap { path =>
      val original = Files.read(path)
      val fixed    = repair(original)
      Option.when(fixed != original)((path, original, fixed))
    }

  /** The check or the repair, as an exit code.
   *
   *  `--check` reports and fails; `--write` repairs in place and succeeds, because a tool
   *  asked to fix something has not failed by fixing it.
   *
   *  @param args `--check` or `--write`, then the paths
   *  @return 1 only when `--check` found damage
   */
  def run(args: Seq[String]): Int =
    val mode  = args.headOption.getOrElse("")
    val paths = args.drop(1)
    // Refusing beats a reassuring "0 file(s) affected" for a command that scanned nothing:
    // the mode and at least one path are both required, and a typo in either is exactly how
    // a guard reports success without having run.
    if (mode != "--check" && mode != "--write") || paths.isEmpty then
      println("usage: fixMojibake --check|--write <path ...>")
      return 2

    val write   = mode == "--write"
    val changed = scan(paths.map(Paths.get(_)))

    changed.foreach { (path, original, fixed) =>
      val lines = original.split("\n", -1).zipAll(fixed.split("\n", -1), "", "").count(_ != _)
      println(s"${if write then "FIX " else "HAS "}$path  ($lines lines)")
      if write then Files.write(path, fixed)
    }

    println(s"\n${changed.size} file(s) ${if write then "repaired" else "affected"}")
    if write || changed.isEmpty then 0 else 1
