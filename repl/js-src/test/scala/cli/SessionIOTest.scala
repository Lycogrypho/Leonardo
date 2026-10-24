package it.grypho.scala.leonardo
package cli

import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

/** The browser backend of `:save` / `:load` against a stub `localStorage` (F_0214).
 *
 *  Assigned through `globalThis`: the linked test bundle is strict-mode, where assigning a bare
 *  undeclared global is a `ReferenceError` (the `FakeDom` rule). */
class SessionIOTest extends AnyFlatSpec with BeforeAndAfter:

  private val store = scala.collection.mutable.Map[String, String]()

  private def install(): Unit =
    global.globalThis.localStorage = js.Dynamic.literal(
      // `getItem` answers JavaScript null for an absent key, as a browser does.
      getItem = (k: String) => store.get(k).orNull,
      setItem = (k: String, v: String) => { store(k) = v }
    )

  after {
    store.clear()
    js.special.delete(global.globalThis, "localStorage")
  }

  "SessionIO" should "say so when there is no storage" in {
    assert(SessionIO.read("s") == Left("browser storage is not available here"))
    assert(!SessionIO.exists("s"))
  }

  it should "report a missing session, and read back a saved one" in {
    install()
    assert(SessionIO.read("s") == Left("no saved session named 's'"))
    assert(!SessionIO.exists("s"))
    assert(SessionIO.write("s", "x := 3") == Right(()))
    assert(SessionIO.exists("s"))
    assert(SessionIO.read("s") == Right("x := 3"))
  }
