package it.grypho.scala.leonardo
package mcp

import io.modelcontextprotocol.json.McpJsonDefaults
import io.modelcontextprotocol.server.{McpServer, McpServerFeatures, McpSyncServer}
import io.modelcontextprotocol.server.transport.StdioServerTransportProvider
import io.modelcontextprotocol.spec.{McpSchema, McpServerTransportProvider}

import java.util.concurrent.CountDownLatch
import scala.concurrent.duration.*
import scala.io.Source
import scala.jdk.CollectionConverters.*


/** The Model Context Protocol wiring (F_0126): the `Tools` catalogue and two resources served over a transport,
 *  through the official Java SDK. */
object StdioServer:

  /** The default wall-clock limit of one request. */
  val DefaultTimeout: FiniteDuration = 30.seconds

  /** The grammar resource's URI. */
  val GrammarUri = "leonardo://grammar"
  /** The session-script resource's URI. */
  val ScriptUri = "leonardo://session/script"

  /** How a client should use the server, sent at initialisation. */
  val Instructions: String =
    "Leonardo is a computer algebra system: symbolic and exact or certified numeric answers. Call " +
    "`evaluate` with one line of its REPL grammar (read the resource " + GrammarUri + " first): " +
    "`name := expr` assigns and is kept for later lines, `=` is an equation, `^` a power, `*` explicit " +
    "multiplication. Prefer delegating any computation to it over computing in-model. An answer it " +
    "cannot give comes back unevaluated with a note saying why; do not treat that as a result."

  private def bundled(name: String): String =
    Option(getClass.getResourceAsStream(s"/leonardo-mcp/$name")).fold("") { in =>
      try Source.fromInputStream(in, "UTF-8").mkString finally in.close()
    }

  /** The server's version, from the build. */
  lazy val version: String = bundled("version.txt").trim match
    case "" => "unknown"
    case v  => v

  /** A tool's JSON schema, an object of string properties, as the plain map the SDK serialises (its
   *  `JsonSchema` record is deprecated since 2.1.0). */
  private def schemaOf(t: ToolDef): java.util.Map[String, Object] =
    val props: java.util.Map[String, Object] = t.params.map(p =>
      p.name -> (Map[String, Object]("type" -> "string", "description" -> p.description).asJava: Object)).toMap.asJava
    Map[String, Object]("type" -> "object", "properties" -> props,
                        "required" -> t.params.filter(_.required).map(_.name).asJava,
                        "additionalProperties" -> java.lang.Boolean.FALSE).asJava

  private def toolSpec(t: ToolDef, worker: SessionWorker): McpServerFeatures.SyncToolSpecification =
    McpServerFeatures.SyncToolSpecification.builder()
      .tool(McpSchema.Tool.builder(t.name, schemaOf(t)).description(t.description).build())
      .callHandler { (_, request) =>
        val args = Option(request.arguments()).fold(Map.empty[String, String])(
          _.asScala.toMap.flatMap((k, v) => Option(v).map(x => k -> x.toString)))
        val answer = Tools.call(t.name, args, worker)
        val b = McpSchema.CallToolResult.builder().addTextContent(answer.text).isError(answer.isError)
        answer.latex.foreach(tex => b.addTextContent(s"LaTeX: $tex"))
        b.build()
      }
      .build()

  private def resourceSpec(uri: String, name: String, mime: String, description: String,
                           text: () => String): McpServerFeatures.SyncResourceSpecification =
    McpServerFeatures.SyncResourceSpecification(
      McpSchema.Resource.builder(uri, name).mimeType(mime).description(description).build(),
      (_, _) => McpSchema.ReadResourceResult.builder(java.util.List.of(
        McpSchema.TextResourceContents.builder(uri, text()).mimeType(mime).build())).build())

  /** The server over a transport, its tools and resources on `worker`'s session.
   *  @param transport the transport
   *  @param worker    the session behind its timeout
   *  @return the running server */
  def serve(transport: McpServerTransportProvider, worker: SessionWorker): McpSyncServer =
    McpServer.sync(transport)
      .serverInfo("leonardo", version)
      .instructions(Instructions)
      .capabilities(McpSchema.ServerCapabilities.builder().tools(false).resources(false, false).build())
      .tools(Tools.all.map(toolSpec(_, worker)).asJava)
      .resources(
        resourceSpec(GrammarUri, "grammar", "text/markdown",
          "Leonardo's cheat sheet: every function and command of the REPL grammar, with examples",
          () => bundled("cheatsheet.md")),
        resourceSpec(ScriptUri, "session script", "text/plain",
          "This session's state as a replayable script (what :save writes)",
          () => worker.run(_.script).fold(why => s"# unavailable: $why", identity)))
      .build()

  /** The ids of the requests read and not yet answered, kept from the lines that pass through the two
   *  streams (F_0373): a request is a message with an `id` and a `method`, its answer one with the same
   *  `id` and no `method`.  Notifications carry no `id`; a line that is not a JSON object is the SDK's to
   *  refuse and is not counted. */
  private final class InFlight:
    private val open = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    private def fields(line: String): Option[java.util.Map[?, ?]] =
      scala.util.Try(McpJsonDefaults.getMapper().readValue(line, classOf[java.util.Map[?, ?]])).toOption.flatMap(Option(_))
    private def idOf(m: java.util.Map[?, ?]): Option[String] = Option(m.get("id")).map(_.toString)
    def read(line: String): Unit = fields(line).foreach(m => if m.containsKey("method") then idOf(m).foreach(open.add))
    def written(line: String): Unit = fields(line).foreach(m => if !m.containsKey("method") then idOf(m).foreach(open.remove))
    def answered: Boolean = open.isEmpty

  /** Splits the bytes passing through a stream into lines, each handed to `line` (UTF-8). */
  private final class Lines(line: String => Unit):
    private val buffer = java.io.ByteArrayOutputStream()
    def bytes(b: Array[Byte], off: Int, len: Int): Unit = synchronized {
      var i = off
      while i < off + len do
        if b(i) == '\n'.toByte then
          line(buffer.toString(java.nio.charset.StandardCharsets.UTF_8))
          buffer.reset()
        else buffer.write(b(i))
        i += 1
    }

  /** Serves over the given streams until the client closes `in` (F_0310), then closes the server.
   *
   *  The transport reads `in` on its own thread, so the end of the input is observed by wrapping it:
   *  the first read that returns end-of-stream releases the wait.  Without it the process outlived its
   *  client — `main` waited on a latch nothing released — and a client that only closes the pipe left
   *  one stray JVM per session.
   *
   *  **The requests already read are answered first** (F_0373): a caller that writes its requests and
   *  closes the input at once (a script, not an assistant) had every answer after `initialize` dropped.
   *  **The end of the input is held back from the transport**, not merely waited for after it: the SDK's
   *  transport stops dispatching once its reader sees end-of-stream, so a wait outside it ran its whole
   *  bound with the tool call never answered (measured, 35 s).  The read that meets the end blocks until
   *  every request read has been answered ([[InFlight]]), bounded by the worker's request limit plus a
   *  margin: every request is answered or refused within that limit of its arrival (F_0316), so the bound
   *  is only reached by a request the transport never answers at all.
   *
   *  @param in     the client's messages
   *  @param out    the server's messages
   *  @param worker the session behind its timeout */
  def runUntilClosed(in: java.io.InputStream, out: java.io.OutputStream, worker: SessionWorker): Unit =
    val closed   = CountDownLatch(1)
    val inFlight = InFlight()
    val inLines  = Lines(inFlight.read)
    val outLines = Lines(inFlight.written)
    def drained(): Unit =
      val deadline = System.nanoTime() + (worker.requestLimit + AnswerMargin).toNanos
      while !inFlight.answered && System.nanoTime() < deadline do Thread.sleep(10)
    val watched = new java.io.FilterInputStream(in):
      private def seen(n: Int): Int =
        if n < 0 then
          drained()
          closed.countDown()
        n
      override def read(): Int =
        val c = seen(super.read())
        if c >= 0 then inLines.bytes(Array(c.toByte), 0, 1)
        c
      override def read(b: Array[Byte], off: Int, len: Int): Int =
        try
          val n = seen(super.read(b, off, len))
          if n > 0 then inLines.bytes(b, off, n)
          n
        catch
          case e: java.io.IOException =>
            closed.countDown()
            throw e
    val sink = out
    val tapped = new java.io.FilterOutputStream(sink):
      override def write(c: Int): Unit =
        sink.write(c)
        outLines.bytes(Array(c.toByte), 0, 1)
      override def write(b: Array[Byte], off: Int, len: Int): Unit =
        sink.write(b, off, len)
        outLines.bytes(b, off, len)
    val server = serve(StdioServerTransportProvider(McpJsonDefaults.getMapper(), watched, tapped), worker)
    try closed.await()
    finally server.closeGracefully()

  /** How long past the worker's request limit the end of the input waits for an answer still missing
   *  (F_0373): time for the SDK to serialise and write one, not a computation's. */
  private val AnswerMargin: FiniteDuration = 5.seconds

  /** Reads `--timeout <seconds>` from the command line.
   *  @param args the arguments
   *  @return the timeout, or why the arguments are not understood */
  def timeoutOf(args: Seq[String]): Either[String, FiniteDuration] = args match
    case Seq()                     => Right(DefaultTimeout)
    case Seq("--timeout", seconds) => seconds.toDoubleOption.filter(s => s > 0 && s.isFinite)
                                        .map(s => (s * 1000).round.millis).toRight(s"not a positive number of seconds: $seconds")
    case other                     => Left(s"usage: leonardo-mcp [--timeout <seconds>]; got ${other.mkString(" ")}")

/** Runs the MCP server over stdio until the client closes it (F_0126).  Logging goes to stderr only:
 *  stdout is the protocol.
 *  @param args `--timeout <seconds>`, optional */
@main def leonardoMcp(args: String*): Unit =
  StdioServer.timeoutOf(args) match
    case Left(why) =>
      System.err.println(why)
      sys.exit(2)
    case Right(timeout) =>
      StdioServer.runUntilClosed(System.in, System.out, SessionWorker(timeout))
      sys.exit(0)
