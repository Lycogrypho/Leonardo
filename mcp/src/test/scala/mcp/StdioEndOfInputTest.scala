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
