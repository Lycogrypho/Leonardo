package it.grypho.scala.leonardo
package mcp

import org.scalatest.flatspec.AnyFlatSpec

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import scala.concurrent.duration.*

/** F_0373: the stdio server answers every request it has read before it returns at the end of its input. */
class StdioEndOfInputTest extends AnyFlatSpec:

  private val requests = Seq(
    """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"t","version":"0"}}}""",
    """{"jsonrpc":"2.0","method":"notifications/initialized"}""",
    """{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"derive","arguments":{"expression":"sin(x)^2","variable":"x"}}}""",
    """{"jsonrpc":"2.0","id":3,"method":"tools/list","params":{}}""")

  "the server" should "answer the requests a client sent before closing its input" in {
    // a batch caller writes its requests and closes stdin at once; the server used to return on the end
    // of input with the tool call still in flight, answering `initialize` alone
    val in  = ByteArrayInputStream(requests.mkString("", "\n", "\n").getBytes(UTF_8))
    val out = ByteArrayOutputStream()
    StdioServer.runUntilClosed(in, out, SessionWorker(30.seconds))
    val answered = out.toString(UTF_8).linesIterator.toVector
    for id <- Seq(1, 2, 3) do
      assert(answered.exists(_.contains(s""""id":$id,"result"""")), s"no answer to $id in:\n${answered.mkString("\n")}")
    assert(answered.exists(_.contains("cos(x)")), answered.mkString("\n"))
  }

  it should "answer a last request with no newline after it (F_0389)" in {
    // the request was counted when its newline passed, so one ended by the end of the input was not waited for
    val in  = ByteArrayInputStream(requests.mkString("\n").getBytes(UTF_8))
    val out = ByteArrayOutputStream()
    StdioServer.runUntilClosed(in, out, SessionWorker(30.seconds))
    val answered = out.toString(UTF_8).linesIterator.toVector
    for id <- Seq(1, 2, 3) do
      assert(answered.exists(_.contains(s""""id":$id,"result"""")), s"no answer to $id in:\n${answered.mkString("\n")}")
  }

  it should "answer a request the client cancelled, which the SDK does not honour (F_0389)" in {
    // the audit reasoned that a cancelled request is never answered and holds the exit for the request limit plus
    // the margin; measured, the SDK registers no handler for notifications/cancelled and answers it, so the wait is
    // right to keep its id.  Should a later SDK skip the answer, this fails: the id must then be dropped on the
    // cancellation
    val slow = Seq(
      requests(0), requests(1),
      """{"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"evaluate","arguments":{"line":"exact precision 1000"}}}""",
      """{"jsonrpc":"2.0","id":8,"method":"tools/call","params":{"name":"evaluate","arguments":{"line":"exact on"}}}""",
      """{"jsonrpc":"2.0","id":9,"method":"tools/call","params":{"name":"evaluate","arguments":{"line":"pi*e*exp(1/3)*ln(7)"}}}""",
      """{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":9,"reason":"test"}}""")
    val in  = ByteArrayInputStream(slow.mkString("", "\n", "\n").getBytes(UTF_8))
    val out = ByteArrayOutputStream()
    val t0 = System.nanoTime()
    StdioServer.runUntilClosed(in, out, SessionWorker(20.seconds))
    val seconds = (System.nanoTime() - t0) / 1e9
    val answered = out.toString(UTF_8).linesIterator.toVector
    val gotNine = answered.exists(_.contains(""""id":9"""))
    info(f"returned after $seconds%.1f s; the cancelled request ${if gotNine then "was" else "was not"} answered")
    assert(gotNine, f"the cancelled request was not answered (returned after $seconds%.1f s)")
  }
