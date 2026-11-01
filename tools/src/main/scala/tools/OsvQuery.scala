package it.grypho.scala.leonardo
package tools

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles, Path, Paths}
import java.time.Duration
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** Asks osv.dev which of the build's own plugin coordinates carry a known vulnerability (F_0001
 *  step 2a, the owner's Decision 2).
 *
 *  **Run by hand before each tag, never in `checks` or CI**: it sends the coordinates to a third
 *  party (`api.osv.dev`), which is a decision for the person releasing, and an answer that depends
 *  on a remote database's state would make a build flaky.  The jars come from the meta-build's
 *  classpath (`project/osv.sbt`), which the `osvPlugins` alias writes before calling this; each is
 *  identified by its Maven or Ivy path, or a flat boot jar by its file name in a cache, and a jar
 *  nothing identifies is printed rather than dropped.
 *
 *  The answer is a report, not a gate: it exits `0` whatever it finds, since most findings in
 *  sbt's own tree have no available upgrade and are judged by hand (the 2026-10-28 table in
 *  `Done.md` ▸ *F_0001 step 2a*), and `1` only when it could not ask — no file, no network, an
 *  answer it cannot read.
 */
object OsvQuery:

  /** A Maven coordinate.
   *  @param group    the group id
   *  @param artifact the artifact id
   *  @param version  the version */
  final case class Coordinate(group: String, artifact: String, version: String):
    override def toString: String = s"$group:$artifact:$version"

  /** OSV's batch endpoint; up to 1000 queries per call. */
  val Endpoint: String = "https://api.osv.dev/v1/querybatch"

  /** The most queries one call carries. */
  val MaxBatch: Int = 1000

  /** A jar in a Maven repository layout (Coursier's cache, a mirror): `…/maven2/g/r/o/up/a/v/a-v.jar`. */
  private val MavenLayout = """.*/(?:maven2|maven-releases)/(.+)/([^/]+)/([^/]+)/\2-\3[^/]*\.jar""".r
  /** A jar in Ivy's cache: `…/.ivy2/cache/g/a/jars/a-v.jar`. */
  private val IvyCache    = """.*/\.ivy2/cache/([^/]+)/([^/]+)/(?:jars|bundles)/\2-(.+)\.jar""".r
  /** A jar in Ivy's local repository: `…/.ivy2/local/g/a/v/jars/a.jar`. */
  private val IvyLocal    = """.*/\.ivy2/local/([^/]+)/([^/]+)/([^/]+)/(?:jars|bundles)/[^/]+\.jar""".r
  /** Scala itself in sbt's boot directory, the version in the directory's name:
   *  `…/boot/scala-2.12.21/lib/scala-library.jar`. */
  private val BootScala   = """.*/boot/scala-([0-9][^/]*)/lib/(scala-library|scala-compiler|scala-reflect)\.jar""".r

  /** The coordinate a jar's path spells, when it lies in a Maven or Ivy layout.
   *  @param path the jar's path, either separator
   *  @return the coordinate, or `None` for a path that names only a file (sbt's boot directory) */
  def coordinateOf(path: String): Option[Coordinate] = path.replace('\\', '/') match
    case MavenLayout(g, a, v) => Some(Coordinate(g.replace('/', '.'), a, v))
    case IvyCache(g, a, v)    => Some(Coordinate(g, a, v))
    case IvyLocal(g, a, v)    => Some(Coordinate(g, a, v))
    case BootScala(v, a)      => Some(Coordinate("org.scala-lang", a, v))
    case _                    => None

  /** Each jar's coordinate: from its own path, else from the path of the same file name in a
   *  cache (`byName`), which is how a jar of sbt's flat boot directory is identified.
   *  @param jars   the classpath's jars
   *  @param byName the cached path of a jar file name, if any
   *  @return the coordinates without repeats, and the jars no layout identified */
  def coordinates(jars: Seq[String], byName: String => Option[String]): (Vector[Coordinate], Vector[String]) =
    val found = jars.map { j =>
      val name = j.replace('\\', '/').split('/').last
      j -> coordinateOf(j).orElse(byName(name).flatMap(coordinateOf))
    }
    (found.flatMap(_._2).toVector.distinct.sortBy(_.toString), found.collect { case (j, None) => j }.toVector)

  /** The jar caches that keep a Maven or Ivy layout, where they exist on this machine. */
  private def cacheRoots: Seq[Path] =
    val home  = sys.props("user.home")
    val local = sys.env.get("LOCALAPPDATA").toSeq.map(l => Paths.get(l, "Coursier", "cache", "v1"))
    (sys.env.get("COURSIER_CACHE").toSeq.map(Paths.get(_)) ++ local ++ Seq(
      Paths.get(home, ".cache", "coursier", "v1"), Paths.get(home, "Library", "Caches", "Coursier", "v1"),
      Paths.get(home, ".ivy2", "cache"), Paths.get(home, ".ivy2", "local"))).filter(JFiles.isDirectory(_))

  /** Every jar under `roots`, by file name, the first found kept. */
  private def cacheIndex(roots: Seq[Path]): Map[String, String] =
    roots.foldLeft(Map.empty[String, String]) { (index, root) =>
      val stream = JFiles.walk(root)
      try
        stream.iterator.asScala.filter(p => p.toString.endsWith(".jar")).foldLeft(index) { (acc, p) =>
          val name = p.getFileName.toString
          if acc.contains(name) then acc else acc.updated(name, p.toString)
        }
      finally stream.close()
    }

  /** A JSON string literal. */
  private def quoted(s: String): String =
    s.flatMap {
      case '"'          => "\\\""
      case '\\'         => "\\\\"
      case c if c < ' ' => f"\\u${c.toInt}%04x"
      case c            => c.toString
    }.mkString("\"", "", "\"")

  /** The batch request for `coordinates`, in OSV's schema.
   *  @param coordinates the coordinates to ask about
   *  @return the JSON body */
  def requestBody(coordinates: Seq[Coordinate]): String =
    coordinates.map { c =>
      s"""{"package":{"ecosystem":"Maven","name":${quoted(s"${c.group}:${c.artifact}")}},"version":${quoted(c.version)}}"""
    }.mkString("""{"queries":[""", ",", "]}")

  /** The top-level objects of the JSON array that follows `"key":` in `json`, as source text;
   *  braces inside strings are ignored. */
  private def objectsOf(json: String, key: String): Option[Vector[String]] =
    val at = json.indexOf(s"\"$key\"")
    val open = if at < 0 then -1 else json.indexOf('[', at)
    Option.when(open >= 0) {
      val found = Vector.newBuilder[String]
      var depth = 0; var start = -1; var inString = false; var escaped = false; var i = open + 1
      var closed = false
      while i < json.length && !closed do
        val c = json.charAt(i)
        if inString then
          if escaped then escaped = false
          else if c == '\\' then escaped = true
          else if c == '"' then inString = false
        else c match
          case '"' => inString = true
          case '{' =>
            if depth == 0 then start = i
            depth += 1
          case '}' =>
            depth -= 1
            if depth == 0 then found += json.substring(start, i + 1)
          case ']' if depth == 0 => closed = true
          case _ => ()
        i += 1
      found.result()
    }

  private val VulnId = """"id"\s*:\s*"([^"]+)"""".r

  /** The advisory ids of each query, in order, read from OSV's batch answer.
   *  @param response the answer's body
   *  @param count    how many queries were sent
   *  @return one list of ids per query, or why the answer could not be read */
  def advisories(response: String, count: Int): Either[String, Vector[Vector[String]]] =
    objectsOf(response, "results") match
      case None                             => Left("the answer has no results array")
      case Some(rs) if rs.size != count     => Left(s"the answer has ${rs.size} results for $count queries")
      case Some(rs)                         => Right(rs.map(r => VulnId.findAllMatchIn(r).map(_.group(1)).toVector.distinct))

  /** Posts one batch and returns the answer's body, or why it failed. */
  private def post(body: String): Either[String, String] =
    Try {
      val client  = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(20)).build()
      val request = HttpRequest.newBuilder(URI.create(Endpoint))
        .timeout(Duration.ofSeconds(60))
        .header("Content-Type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(body, StandardCharsets.UTF_8))
        .build()
      client.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
    }.toEither.left.map(e => s"the request failed: $e").flatMap { r =>
      if r.statusCode == 200 then Right(r.body) else Left(s"osv.dev answered HTTP ${r.statusCode}")
    }

  /** Reads the classpath file named by `args` (default `target/osv/plugin-classpath.txt`), maps its
   *  jars to coordinates, asks osv.dev, and prints every coordinate with an advisory and every jar
   *  it could not identify.  With `--dry-run` it prints the coordinates it would send and asks no one.
   *  @param args at most one path, and `--dry-run`
   *  @return `0` when the question was answered (or not asked, dry), `1` when it could not be asked */
  def run(args: Seq[String]): Int =
    val dry  = args.contains("--dry-run")
    val path = Paths.get(args.filterNot(_ == "--dry-run").headOption.getOrElse("target/osv/plugin-classpath.txt"))
    if !JFiles.isRegularFile(path) then
      println(s"$path not found: run `sbt osvPlugins`, which writes it first")
      1
    else
      val jars  = JFiles.readAllLines(path, StandardCharsets.UTF_8).asScala.toSeq.map(_.trim).filter(_.nonEmpty)
      lazy val index = cacheIndex(cacheRoots)
      val (coordinates, unmatched) = OsvQuery.coordinates(jars, name => index.get(name))
      println(s"${jars.size} jars, ${coordinates.size} coordinates; ${unmatched.size} jars no layout identifies")
      unmatched.foreach(j => println(s"  not asked: $j"))
      if dry then
        coordinates.foreach(c => println(s"  would ask: $c"))
        return 0
      val answers = coordinates.grouped(MaxBatch).toVector.foldLeft[Either[String, Vector[Vector[String]]]](Right(Vector.empty)) {
        (acc, batch) => acc.flatMap(sofar => post(requestBody(batch)).flatMap(advisories(_, batch.size)).map(sofar ++ _))
      }
      answers match
        case Left(why) =>
          println(s"osv.dev was not asked successfully: $why")
          1
        case Right(ids) =>
          val hits = coordinates.zip(ids).filter(_._2.nonEmpty)
          println(s"${coordinates.size} coordinates asked; ${hits.size} with a known advisory")
          hits.foreach((c, vs) => println(s"  $c: ${vs.mkString(", ")}"))
          if hits.nonEmpty then println("  details: https://osv.dev/vulnerability/<id>")
          0
