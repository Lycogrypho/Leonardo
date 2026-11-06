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
   *  `JsonSchema` record is deprecated in 2.1.0). */
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

  /** Serves over the given streams until the client closes `in` (F_0310), then closes the server.
   *
   *  The transport reads `in` on its own thread, so the end of the input is observed by wrapping it:
   *  the first read that returns end-of-stream releases the wait.  Without it the process outlived its
   *  client — `main` waited on a latch nothing released — and a client that only closes the pipe left
   *  one stray JVM per session.
   *
   *  @param in     the client's messages
   *  @param out    the server's messages
   *  @param worker the session behind its timeout */
  def runUntilClosed(in: java.io.InputStream, out: java.io.OutputStream, worker: SessionWorker): Unit =
    val closed = CountDownLatch(1)
    val watched = new java.io.FilterInputStream(in):
      private def seen(n: Int): Int =
        if n < 0 then closed.countDown()
        n
      override def read(): Int = seen(super.read())
      override def read(b: Array[Byte], off: Int, len: Int): Int =
        try seen(super.read(b, off, len))
        catch
          case e: java.io.IOException =>
            closed.countDown()
            throw e
    val server = serve(StdioServerTransportProvider(McpJsonDefaults.getMapper(), watched, out), worker)
    try closed.await()
    finally server.closeGracefully()

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
