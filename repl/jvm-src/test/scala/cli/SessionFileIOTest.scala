package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter


/** `:save` / `:load` against the real file system -- JVM ONLY (issue F_0003 phase 2).
 *
 *  Split out of `ReplSessionTest` when `repl` became a cross-build.  These cases exercise the
 *  JVM's `SessionIO`, not `Session`: they create temporary files, which a browser cannot do.
 *  Everything else in the REPL suite is platform-neutral and still runs on both.
 *
 *  The browser's counterpart is `localStorage`, reached through the same `:save` / `:load`
 *  commands -- so the behaviour pinned here is the contract, and only the medium differs.
 */
class SessionFileIOTest extends AnyFlatSpec with BeforeAndAfter:

  private var session: Session = _

  before { session = new Session() }

  "saveFile then loadFile" should "round-trip session state through a real file" in
  {
    val tmp = java.io.File.createTempFile("leonardo-session", ".txt")
    tmp.deleteOnExit()
    try
      val original = session
      original.execute("precision 6")
      original.execute("k := 7")
      original.execute("h := k + x")
      assert(Session.saveFile(original, tmp.getPath) == s"saved to ${tmp.getPath}")

      val restored = session
      val out = Session.loadFile(restored, tmp.getPath)
      assert(!out.startsWith("could not read"), out)
      assert(restored.execute("k") == "7.0")
      restored.execute("x := 1")
      assert(restored.execute("h") == "8.0")
    finally tmp.delete()
  }

  "Session.saveFile / loadFile" should "round-trip the session state through a temp file" in
  {
    val s1 = session
    s1.execute("precision 8")
    s1.execute("x := 3.00000001")
    s1.execute("g := sin(y) + y")   // y is unbound → stays a definition after reload
    val tmp = java.io.File.createTempFile("leonardo_test", ".leo")
    try
      val saveMsg = Session.saveFile(s1, tmp.getPath)
      assert(saveMsg.startsWith("saved"), s"save failed: $saveMsg")
      val s2 = session
      Session.loadFile(s2, tmp.getPath)
      assert(s2.execute("env").contains("precision = 8"))
      assert(s2.execute("x") == "3.00000001")
      assert(s2.execute("g") == "(sin(y) + y)")
    finally
      tmp.delete()
  }

  "colors setting" should "round-trip through saveFile / loadFile" in
  {
    val tmp = java.io.File.createTempFile("leonardo-colors", ".txt")
    tmp.deleteOnExit()
    try
      val s1 = session
      s1.execute("colors none")
      Session.saveFile(s1, tmp.getPath)
      val s2 = session
      Session.loadFile(s2, tmp.getPath)
      assert(s2.currentColorScheme == "none")
    finally tmp.delete()
  }

  "step on :load and :save" should "perform the file IO the bare execute path refuses" in
  {
    val tmp = java.io.File.createTempFile("leonardo-step", ".txt")
    tmp.deleteOnExit()
    try
      val s1 = session
      s1.execute("a := 9")
      val saved = Session.step(s1, Some(s":save ${tmp.getPath}"))
      assert(saved.exists(_.startsWith("saved to")), s"expected save confirmation, got: $saved")

      val s2 = session
      val loaded = Session.step(s2, Some(s":load ${tmp.getPath}"))
      assert(loaded.exists(!_.startsWith("could not read")), s"expected a successful load, got: $loaded")
      assert(s2.execute("a") == "9.0")
      // contrast: the same tokens through execute are refused as interactive-only
      assert(s2.execute(":save foo.txt").contains("interactive"))
    finally tmp.delete()
  }

