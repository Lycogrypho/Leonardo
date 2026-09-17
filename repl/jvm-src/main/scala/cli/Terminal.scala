package it.grypho.scala.leonardo
package cli

import org.jline.reader.{EndOfFileException, LineReader, LineReaderBuilder, Reference, UserInterruptException}
import org.jline.terminal.TerminalBuilder

import scala.util.control.NonFatal

/* The interactive read loop and its key bindings -- JVM ONLY (issue F_0003 phase 2).
 *
 * This is everything in the REPL that a browser cannot have: JLine's line editor, the Greek
 * insertion chords it installs, and the terminal itself.  `Session`, which holds all of the
 * actual REPL behaviour, stayed in the shared tree because it never touched JLine -- that was
 * true before the cross-build and is what made phase 2 cheap.
 */

/** The escape byte an `ALT-<key>` press sends; built from its code point rather than
 *  written literally so the source stays free of control characters.
 */
private val Esc: String = 27.toChar.toString

/** The Greek symbols reachable from the `ALT-\` prefix, paired with the letter that
 *  selects them: `ALT-\ g` inserts `Γ`, `ALT-\ b` inserts `β`.
 *
 *  A **prefix** scheme rather than one direct chord per symbol, for two reasons.  `ALT-b`
 *  is already `backward-word` in JLine's emacs key map -- and `ALT-B` redirects to it via
 *  `do-lowercase-version`, so the capital does not dodge the clash -- while a prefix
 *  extends to further symbols without ever having to re-check for a collision.  `ALT-\`
 *  itself is verified unbound.
 *
 *  Only symbols the grammar actually accepts belong here: the variable regex is ASCII-only,
 *  so inserting a character the parser cannot read would just manufacture parse errors.
 *  Today that means the two special-function aliases and nothing else.
 */
private[cli] val GreekChords: List[(Char, String)] = List('g' -> "Γ", 'b' -> "β")

/** Key sequences JLine's emacs map already owns, from a direct dump of that map.
 *
 *  Kept as data so the "we never steal an editing shortcut" rule is a *testable* claim
 *  rather than a comment.  `ALT-B` is absent on purpose: it resolves to `ALT-b` through
 *  `do-lowercase-version`, so binding the capital would take `backward-word` just the same.
 */
private[cli] val EmacsReservedChords: Map[String, String] = Map(
  "a" -> "accept-and-hold", "b" -> "backward-word",  "c" -> "capitalize-word",
  "d" -> "kill-word",       "f" -> "forward-word")

/** The chords to install, as `(key sequence, widget name)` pairs.
 *
 *  Pure, and deliberately so: the invariant worth guarding -- that none of these collides
 *  with an editing shortcut -- is a property of this list, not of a live terminal.  Testing
 *  it here means the suite never has to build a JLine `Terminal`, which loads a native
 *  library and, inside sbt's in-process runner, intermittently crashed the JVM with an
 *  EXCEPTION_ACCESS_VIOLATION during native library load.
 */
private[cli] def greekKeySequences: List[(String, String)] =
  GreekChords.map((letter, _) => (s"$Esc\\$letter", s"insert-greek-$letter")) :+
    // ALT-G as well: verified free, and the direct shortcut for the commoner symbol.
    ((s"${Esc}g", "insert-greek-g"))

/** Binds the Greek insertion chords on `reader`, leaving every existing binding intact.
 *
 *  Thin glue over [[greekKeySequences]] and [[GreekChords]]; `repl()`'s interactive loop
 *  cannot be exercised by the suite, so the decisions live in those two values where they
 *  can be, and this function only applies them.
 *
 *  The key map is looked up through `Option`: `getKeyMaps` is a plain `java.util.Map`, so
 *  its `get` yields `null` for a reader without a MAIN map, and binding through that
 *  reference would fail REPL start-up with an NPE.  A session without the chords is
 *  strictly better than one that will not start, so an absent map skips the binding step
 *  and the widgets are registered regardless.
 *
 *  @param reader the line reader whose MAIN key map is extended
 */
private[cli] def installGreekChords(reader: LineReader): Unit =
  for (letter, symbol) <- GreekChords do
    reader.getWidgets.put(s"insert-greek-$letter", () => { reader.getBuffer.write(symbol); true })
  for
    keyMap        <- Option(reader.getKeyMaps.get(LineReader.MAIN))
    (seq, widget) <- greekKeySequences
  do keyMap.bind(new Reference(widget), seq)


/** Entry point for the interactive Leonardo REPL. */
@main def repl(): Unit =
  val session = Session()
  // A system terminal enables arrow-key line editing; dumb(true) makes it fall back
  // gracefully (rather than throwing) when no interactive console is attached, e.g.
  // piped input or CI, where the loop still works line-by-line without editing.
  val terminal = TerminalBuilder.builder().system(true).dumb(true).build()
  // Command history persisted across sessions in the user's home directory; JLine
  // loads it on start and appends to it as lines are entered.
  val historyFile = java.nio.file.Paths.get(System.getProperty("user.home"), ".leonardo_history")
  val highlighter = LeonardoHighlighter(() => session.currentColorScheme)
  val reader = LineReaderBuilder.builder()
    .terminal(terminal)
    .variable(LineReader.HISTORY_FILE, historyFile)
    .highlighter(highlighter)
    .build()
  installGreekChords(reader)
  val out = terminal.writer()
  out.println("Leonardo CAS -- type 'help' for commands, 'quit' to leave")
  out.flush()
  try
    var running = true
    while running do
      // Ctrl-C abandons the current line but keeps the session (empty line = no-op);
      // Ctrl-D (end of input) ends the loop like `quit`.
      val line =
        try Some(reader.readLine("leonardo> "))
        catch
          case _: UserInterruptException => Some("")
          case _: EndOfFileException     => None
      Session.step(session, line) match
        case None         => running = false
        case Some(result) => if result.nonEmpty then out.println(result)
      out.flush()
  finally
    terminal.close()
