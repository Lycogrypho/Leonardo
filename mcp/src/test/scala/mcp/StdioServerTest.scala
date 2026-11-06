package it.grypho.scala.leonardo
package mcp

import io.modelcontextprotocol.json.McpJsonDefaults
import io.modelcontextprotocol.server.transport.StdioServerTransportProvider
import org.scalatest.flatspec.AnyFlatSpec

import java.io.{BufferedReader, InputStreamReader, PipedInputStream, PipedOutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.duration.*

/** The server end to end over the stdio transport, in-process through pipes: the JSON-RPC lines a
 *  client sends and the ones it reads back. */
class StdioServerTest extends AnyFlatSpec:

  "the stdio server" should "initialise, list its tools, answer a call and serve the grammar" in {
    val toServer   = PipedOutputStream()
    val serverIn   = PipedInputStream(toServer, 1 << 16)
    val fromServer = PipedInputStream(1 << 20)
    val serverOut  = PipedOutputStream(fromServer)
    val server = StdioServer.serve(
      StdioServerTransportProvider(McpJsonDefaults.getMapper(), serverIn, serverOut), SessionWorker(30.seconds))
    val send   = PrintStream(toServer, true, UTF_8)
    val reader = BufferedReader(InputStreamReader(fromServer, UTF_8))
    val pool   = Executors.newSingleThreadExecutor()
    /** The response to request `id`, skipping any notification the server sends meanwhile. */
    def response(id: Int): String =
      pool.submit(() =>
        Iterator.continually(Option(reader.readLine())).takeWhile(_.isDefined).flatten
          .find(_.contains(s"\"id\":$id")).getOrElse("")
      ).get(20, TimeUnit.SECONDS)
    try
      send.println("""{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test","version":"0"}}}""")
      val init = response(1)
      assert(init.contains("\"leonardo\"") && init.contains("instructions"), init)
      send.println("""{"jsonrpc":"2.0","method":"notifications/initialized"}""")

      send.println("""{"jsonrpc":"2.0","id":2,"method":"tools/list"}""")
      val tools = response(2)
      for t <- Tools.all do assert(tools.contains(s"\"${t.name}\""), t.name)

      send.println("""{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"evaluate","arguments":{"line":"2 + 3"}}}""")
      val answer = response(3)
      assert(answer.contains("5.0") && answer.contains("\"isError\":false"), answer)

      send.println("""{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"derive","arguments":{"expression":"x^2"}}}""")
      val refused = response(4)
      assert(refused.contains("\"isError\":true") || refused.contains("\"error\""), refused)

      send.println("""{"jsonrpc":"2.0","id":5,"method":"resources/read","params":{"uri":"leonardo://grammar"}}""")
      val grammar = response(5)
      assert(grammar.contains("derive") && grammar.length > 2000, grammar.take(200))
    finally
      server.close()
      pool.shutdownNow()
  }

  "runUntilClosed" should "return once the client closes the server's input (F_0310)" in {
    val toServer  = PipedOutputStream()
    val serverIn  = PipedInputStream(toServer, 1 << 16)
    val serverOut = java.io.ByteArrayOutputStream()
    val done = java.util.concurrent.CountDownLatch(1)
    val runner = Thread(() => { StdioServer.runUntilClosed(serverIn, serverOut, SessionWorker(30.seconds)); done.countDown() })
    runner.setDaemon(true)
    runner.start()
    val send = PrintStream(toServer, true, UTF_8)
    send.println("""{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"eof","version":"0"}}}""")
    Thread.sleep(500)
    assert(done.getCount == 1, "returned before its input closed")
    send.close()
    assert(done.await(10, TimeUnit.SECONDS), "still running 10 s after its input closed")
  }

  "--timeout" should "be read in seconds and refuse anything else" in {
    assert(StdioServer.timeoutOf(Seq()) == Right(StdioServer.DefaultTimeout))
    assert(StdioServer.timeoutOf(Seq("--timeout", "2.5")) == Right(2500.millis))
    assert(StdioServer.timeoutOf(Seq("--timeout", "0")).isLeft)
    assert(StdioServer.timeoutOf(Seq("--verbose")).isLeft)
  }
