package it.grypho.scala.leonardo
package tools

import java.nio.file.{Files as JFiles, Path, Paths}

/** Reads the architecture diagram, `docs/structure.puml`, as a graph and writes it as GraphML
 *  for Gephi (F_0097).
 *
 *  PlantUML has no GraphML exporter, and converting its SVG would be fragile, so the source is
 *  read directly. **The reader knows exactly the subset the diagram uses**, a line-based one:
 *  `@startuml`/`@enduml`, `!define`, `skinparam` (one line or a block), `hide`, comments,
 *  `legend … endlegend`, nested `package "p" [<<s>>] {`, declarations
 *  `(abstract class|class|interface|enum) "Name" <<s>>… [[url]] [{ members }]`, and relations
 *  `"A" arrow "B" [: label]` with the four arrows the diagram draws.
 *
 *  **Anything else is refused, naming the line, never dropped**: an unknown arrow or keyword, a
 *  duplicate declaration, an edge to an undeclared name, an unclosed or unmatched block. That
 *  is what makes the reader a standing check (Decision B): PlantUML accepts all of those
 *  silently, which is how D_0096's four defects decayed unseen.
 *
 *  Packages become a node **attribute**, not group nodes: Gephi flattens nested graphs, and an
 *  attribute is what its partition colouring reads. The output carries no colour: colour by
 *  partition on `package` or `stereotype` in Gephi.
 */
object PumlGraph:

  /** The diagram, and its committed conversion (Decision A: committed beside it). */
  val DefaultSource: Path = Paths.get("docs", "structure.puml")
  val DefaultTarget: Path = Paths.get("docs", "structure.graphml")
  /** The yEd conversion (F_0098 Decision B: a second output of the same run, committed). */
  val DefaultYEdTarget: Path = Paths.get("docs", "structure.yed.graphml")

  /** What a relation means, by its arrow. */
  enum Relation(val arrow: String, val word: String):
    case Dependency  extends Relation("..>", "dependency")
    case Inheritance extends Relation("--|>", "inheritance")
    case Realisation extends Relation("..|>", "realisation")
    case Association extends Relation("-->", "association")

  /** One declaration.
   *  @param name       the quoted name, unique in the diagram
   *  @param pkg        the enclosing packages, outermost first, joined by `.`; empty at top level
   *  @param kind       `class`, `abstract class`, `interface` or `enum`
   *  @param stereotype the stereotypes, joined by `, `
   *  @param url        the `[[…]]` link, or empty
   *  @param members    the body's lines, trimmed, separators included
   *  @param line       where it is declared, from 1
   */
  final case class Node(name: String, pkg: String, kind: String, stereotype: String, url: String,
                        members: Vector[String], line: Int)

  /** One relation, `source arrow target`.
   *  @param line where it is written, from 1 */
  final case class Edge(source: String, target: String, relation: Relation, label: String, line: Int)

  /** The diagram as a graph, in source order.
   *  @param styles the `skinparam` blocks' entries, `(element, stereotype, key) -> value`, the
   *                stereotype empty for an unqualified entry: the colour legend the yEd output
   *                reads (F_0098), so the legend stays the one definition of the colours */
  final case class Diagram(nodes: Vector[Node], edges: Vector[Edge],
                           styles: Map[(String, String, String), String] = Map.empty)

  private val Declaration =
    """^(abstract class|class|interface|enum)\s+"([^"]+)"((?:\s+<<[^>]+>>)*)(?:\s+\[\[([^\]]+)\]\])?\s*(\{)?$""".r
  private val Stereotype = """<<([^>]+)>>""".r
  private val Package    = """^package\s+"([^"]+)"(?:\s+<<[^>]+>>)?\s*\{$""".r
  private val Link       = """^"([^"]+)"\s+(\S+)\s+"([^"]+)"(?:\s*:\s*(.*))?$""".r
  private val Directive  = """^(@startuml|@enduml|!define\s.*|hide\s.*|skinparam\s+[^{]*)$""".r
  private val BlockOpen  = """^skinparam\s+(\w+)\s*\{$""".r
  private val StyleEntry = """^(\w+)(?:<<([^>]+)>>)?\s+(\S+)$""".r

  /** Reads `text` as the diagram subset, or answers every refusal, each naming its line.
   *
   *  @param text the `.puml` source
   *  @return the diagram, or the refusals
   */
  def read(text: String): Either[Vector[String], Diagram] =
    val lines  = text.stripPrefix("﻿").linesIterator.toVector
    val errors = Vector.newBuilder[String]
    val nodes  = Vector.newBuilder[Node]
    val edges  = Vector.newBuilder[Edge]
    var packages = List.empty[(String, Int)]            // innermost first, with the opening line
    var body     = Option.empty[(Node, Vector[String])] // a declaration whose `{` is open
    var block    = Option.empty[(String, Int)]          // the closing word and the opening line
    var styling  = Option.empty[String]                 // the element of an open skinparam block
    val styles   = Map.newBuilder[(String, String, String), String]

    for (raw, i) <- lines.zipWithIndex do
      val n = i + 1
      val t = raw.trim
      (body, block) match
        case (Some((node, members)), _) =>
          if t == "}" then { nodes += node.copy(members = members); body = None }
          else if t.nonEmpty then body = Some((node, members :+ t))
        case (None, Some((close, _))) =>
          if t == close then { block = None; styling = None }
          else styling.foreach { element =>
            t match
              case StyleEntry(key, stereo, value) => styles += ((element, Option(stereo).getOrElse("").trim, key) -> value)
              case _ =>
          }
        case _ =>
          t match
            case "" =>
            case c if c.startsWith("'") =>
            case BlockOpen(element) => block = Some(("}", n)); styling = Some(element)
            case l if l == "legend" || l.startsWith("legend ") => block = Some(("endlegend", n))
            case Directive(_)   =>
            case Package(name)  => packages = (name, n) :: packages
            case "}"            =>
              if packages.isEmpty then errors += s"line $n: a closing brace with no open package"
              else packages = packages.tail
            case Declaration(kind, name, stereo, url, open) =>
              val node = Node(name, packages.reverse.map(_._1).mkString("."), kind,
                              Stereotype.findAllMatchIn(Option(stereo).getOrElse("")).map(_.group(1).trim).mkString(", "),
                              Option(url).getOrElse(""), Vector.empty, n)
              if Option(open).isDefined then body = Some((node, Vector.empty)) else nodes += node
            case Link(source, arrow, target, label) =>
              Relation.values.find(_.arrow == arrow) match
                case Some(r) => edges += Edge(source, target, r, Option(label).getOrElse("").trim, n)
                case None    => errors += s"line $n: unknown arrow '$arrow' in: $t"
            case other => errors += s"line $n: not part of the diagram subset this reader knows: $other"

    body.foreach((node, _) => errors += s"line ${node.line}: the body of \"${node.name}\" is not closed")
    block.foreach((close, at) => errors += s"line $at: a block is not closed (expected '$close')")
    packages.foreach((name, at) => errors += s"line $at: package \"$name\" is not closed")

    val ns = nodes.result()
    val es = edges.result()
    val first = ns.groupBy(_.name).view.mapValues(_.minBy(_.line)).toMap
    ns.foreach { node =>
      first.get(node.name).filter(_.line != node.line).foreach { f =>
        errors += s"line ${node.line}: \"${node.name}\" is already declared at line ${f.line}"
      }
    }
    es.foreach { e =>
      List(e.source, e.target).filterNot(first.contains).foreach { name =>
        errors += s"line ${e.line}: the relation names \"$name\", which is not declared"
      }
    }
    val all = errors.result()
    if all.isEmpty then Right(Diagram(ns, es, styles.result())) else Left(all)

  /** Escapes text for an XML element or attribute. */
  private def escape(s: String): String =
    s.flatMap {
      case '&' => "&amp;"
      case '<' => "&lt;"
      case '>' => "&gt;"
      case '"' => "&quot;"
      case c   => c.toString
    }

  private val NodeKeys = Vector("label", "package", "kind", "stereotype", "url", "members")
  private val EdgeKeys = Vector("relation", "label")

  /** The diagram as GraphML: one node per declaration and one directed edge per relation, in
   *  source order, with typed string attributes. Deterministic, so a committed copy can be
   *  compared against a fresh conversion. Empty attributes are omitted.
   *
   *  @param d the diagram
   *  @return the GraphML document, `\n`-terminated lines
   */
  def toGraphML(d: Diagram): String =
    val ids = d.nodes.zipWithIndex.map((node, i) => node.name -> s"n$i").toMap
    def data(key: String, value: String): Option[String] =
      Option.when(value.nonEmpty)(s"""      <data key="$key">${escape(value)}</data>""")
    val keys =
      NodeKeys.map(k => s"""  <key id="n_$k" for="node" attr.name="$k" attr.type="string"/>""") ++
      EdgeKeys.map(k => s"""  <key id="e_$k" for="edge" attr.name="$k" attr.type="string"/>""")
    val nodes = d.nodes.flatMap { node =>
      val values = Vector(node.name, node.pkg, node.kind, node.stereotype, node.url, node.members.mkString("\n"))
      Vector(s"""    <node id="${ids(node.name)}">""") ++
        NodeKeys.zip(values).flatMap((k, v) => data(s"n_$k", v).map("  " + _)) :+ "    </node>"
    }
    val edges = d.edges.zipWithIndex.flatMap { (e, i) =>
      Vector(s"""    <edge id="e$i" source="${ids(e.source)}" target="${ids(e.target)}">""") ++
        EdgeKeys.zip(Vector(e.relation.word, e.label)).flatMap((k, v) => data(s"e_$k", v).map("  " + _)) :+
        "    </edge>"
    }
    (Vector(
      """<?xml version="1.0" encoding="UTF-8"?>""",
      "<!-- Generated from docs/structure.puml by `sbt graphml` (F_0097). Do not edit:",
      "     `sbt checks` compares this file against a fresh conversion. -->",
      """<graphml xmlns="http://graphml.graphdrawing.org/xmlns">""") ++ keys ++
      Vector("""  <graph id="leonardo" edgedefault="directed">""") ++ nodes ++ edges ++
      Vector("  </graph>", "</graphml>")).mkString("", "\n", "\n")

  /** A `skinparam` colour as yEd reads it, `#RRGGBB`: a hex value as written, or one of the sixteen
   *  basic colour names.  `None` for anything else, which the repository test refuses, so a new
   *  legend entry yEd could not read fails there rather than turning white in the drawing.
   *  @param value the colour as the legend writes it
   *  @return the hex form, or `None`
   */
  def colour(value: String): Option[String] =
    if value.matches("#[0-9A-Fa-f]{6}") then Some(value.toUpperCase)
    else BasicColours.get(value.toLowerCase)

  private val BasicColours = Map(
    "white" -> "#FFFFFF", "black" -> "#000000", "gray" -> "#808080", "silver" -> "#C0C0C0",
    "red" -> "#FF0000", "maroon" -> "#800000", "yellow" -> "#FFFF00", "olive" -> "#808000",
    "lime" -> "#00FF00", "green" -> "#008000", "aqua" -> "#00FFFF", "teal" -> "#008080",
    "blue" -> "#0000FF", "navy" -> "#000080", "fuchsia" -> "#FF00FF", "purple" -> "#800080")

  /** Where a box's relative `[[page.html]]` link points: the published site, so it opens from yEd. */
  val SiteRoot = "https://lycogrypho.github.io/Leonardo/"

  /** How each relation is drawn in yEd: the line and the target arrow, PlantUML's own forms --
   *  dashed for a dependency and a realisation, a hollow triangle for inheritance and realisation. */
  private def drawn(r: Relation): (String, String) = r match
    case Relation.Dependency  => ("dashed", "plain")
    case Relation.Inheritance => ("line", "white_delta")
    case Relation.Realisation => ("dashed", "white_delta")
    case Relation.Association => ("line", "plain")

  /** The diagram as yEd GraphML (F_0098): one `y:ProxyAutoBoundsNode` group per package, nested by
   *  the package path; one `y:UMLClassNode` per declaration, its members split into attributes and
   *  operations (a member with a `(`), coloured from the `skinparam` legend by its first stereotype
   *  that has an entry; one `y:PolyLineEdge` per relation with its line, arrow and label; a box's
   *  link as a `url` datum on the published site.
   *
   *  **Written to the shape `pyyed` emits** (Decision A: public yEd files, cited in `Done.md`), the
   *  generator whose output yEd opens; the owner opens the result in yEd once.  No layout is
   *  computed: every box sits at the origin with a size fitted to its text, and yEd's
   *  *Layout ▸ Hierarchical* places them, as Graphviz does for the SVG.  Ids follow yEd's own
   *  convention, a nested node's id prefixed by its group's (`n0::n3`).  Deterministic.
   *
   *  @param d the diagram
   *  @return the yEd GraphML document, `\n`-terminated lines
   */
  def toYEd(d: Diagram): String =
    def style(element: String, stereotypes: List[String], key: String, fallback: String): String =
      stereotypes.iterator.flatMap(s => d.styles.get((element, s, key))).nextOption()
        .orElse(d.styles.get((element, "", key))).flatMap(colour).getOrElse(fallback)

    // The packages as a tree, in order of first appearance.
    val paths = d.nodes.map(_.pkg).filter(_.nonEmpty).flatMap { p =>
      val parts = p.split('.').toVector
      parts.indices.map(i => parts.take(i + 1).mkString("."))
    }.distinct
    def children(parent: String): Vector[String] =
      paths.filter(p => if parent.isEmpty then !p.contains('.') else p.startsWith(parent + ".") && !p.drop(parent.length + 1).contains('.'))

    val out   = Vector.newBuilder[String]
    val ids   = scala.collection.mutable.LinkedHashMap.empty[String, String]   // node name -> id
    val arrow = style("class", Nil, "ArrowColor", "#444466")

    def classNode(node: Node, id: String, indent: String): Unit =
      ids(node.name) = id
      val stereos   = node.stereotype.split(", ").toList.filter(_.nonEmpty)
      val element   = if node.kind == "interface" then "interface" else "class"
      val fill      = style(element, stereos, "BackgroundColor", "#FFFFFF")
      val border    = style(element, stereos, "BorderColor", "#555577")
      val (ops, attrs) = node.members.map(m => if m.startsWith("__") && m.endsWith("__") then s"-- ${m.stripPrefix("__").stripSuffix("__").trim} --" else m)
        .partition(_.contains("("))
      val label     = Vector(node.stereotype, if node.kind == "class" then "" else node.kind).filter(_.nonEmpty).mkString(", ")
      val lines     = 1 + attrs.size + ops.size + (if label.nonEmpty then 1 else 0)
      val longest   = (node.name +: (attrs ++ ops)).map(_.length).max
      val (w, h)    = (math.max(80, longest * 8 + 20), 28 + 16 * lines)
      out += s"""$indent<node id="$id">"""
      if node.url.nonEmpty then
        val url = if node.url.contains("://") then node.url else SiteRoot + node.url
        out += s"""$indent  <data key="d1">${escape(url)}</data>"""
      out += s"""$indent  <data key="d0">"""
      out += s"""$indent    <y:UMLClassNode>"""
      out += s"""$indent      <y:Geometry height="$h.0" width="$w.0" x="0.0" y="0.0"/>"""
      out += s"""$indent      <y:Fill color="$fill" transparent="false"/>"""
      out += s"""$indent      <y:BorderStyle color="$border" type="line" width="1.0"/>"""
      out += s"""$indent      <y:NodeLabel modelName="internal" modelPosition="c">${escape(node.name)}</y:NodeLabel>"""
      out += s"""$indent      <y:UML stereotype="${escape(label)}">"""
      out += s"""$indent        <y:AttributeLabel>${escape(attrs.mkString("\n"))}</y:AttributeLabel>"""
      out += s"""$indent        <y:MethodLabel>${escape(ops.mkString("\n"))}</y:MethodLabel>"""
      out += s"""$indent      </y:UML>"""
      out += s"""$indent    </y:UMLClassNode>"""
      out += s"""$indent  </data>"""
      out += s"""$indent</node>"""

    /** The members of the graph `parent` (a package path, empty at the top): its classes, then its
     *  sub-packages as groups, each id prefixed by `prefix`. */
    def graph(parent: String, prefix: String, indent: String): Unit =
      val classes = d.nodes.filter(_.pkg == parent)
      classes.zipWithIndex.foreach((node, i) => classNode(node, s"${prefix}n$i", indent))
      children(parent).zipWithIndex.foreach { (pkg, j) =>
        val id = s"${prefix}n${classes.size + j}"
        out += s"""$indent<node id="$id" yfiles.foldertype="group">"""
        out += s"""$indent  <data key="d0">"""
        out += s"""$indent    <y:ProxyAutoBoundsNode>"""
        out += s"""$indent      <y:Realizers active="0">"""
        out += s"""$indent        <y:GroupNode>"""
        out += s"""$indent          <y:Fill color="${style("package", Nil, "BackgroundColor", "#F4F4FF")}" transparent="false"/>"""
        out += s"""$indent          <y:BorderStyle color="${style("package", Nil, "BorderColor", "#7777AA")}" type="line" width="1.0"/>"""
        out += s"""$indent          <y:NodeLabel modelName="internal" modelPosition="t">${escape(pkg.split('.').last)}</y:NodeLabel>"""
        out += s"""$indent          <y:Shape type="rectangle"/>"""
        out += s"""$indent          <y:State closed="false"/>"""
        out += s"""$indent        </y:GroupNode>"""
        out += s"""$indent      </y:Realizers>"""
        out += s"""$indent    </y:ProxyAutoBoundsNode>"""
        out += s"""$indent  </data>"""
        out += s"""$indent  <graph edgedefault="directed" id="$id:">"""
        graph(pkg, s"$id::", indent + "    ")
        out += s"""$indent  </graph>"""
        out += s"""$indent</node>"""
      }

    graph("", "", "    ")
    // Edges at the top level, which contains both ends of every one (GraphML's rule for an edge
    // between nodes of different groups).
    d.edges.zipWithIndex.foreach { (e, i) =>
      val (line, head) = drawn(e.relation)
      out += s"""    <edge id="e$i" source="${ids(e.source)}" target="${ids(e.target)}">"""
      out += s"""      <data key="d3">"""
      out += s"""        <y:PolyLineEdge>"""
      out += s"""          <y:LineStyle color="$arrow" type="$line" width="1.0"/>"""
      out += s"""          <y:Arrows source="none" target="$head"/>"""
      if e.label.nonEmpty then out += s"""          <y:EdgeLabel>${escape(e.label)}</y:EdgeLabel>"""
      out += s"""        </y:PolyLineEdge>"""
      out += s"""      </data>"""
      out += s"""    </edge>"""
    }

    (Vector(
      """<?xml version="1.0" encoding="UTF-8" standalone="no"?>""",
      "<!-- Generated from docs/structure.puml by `sbt graphml` (F_0098), for yEd: open it and apply",
      "     Layout > Hierarchical. Do not edit: `sbt checks` compares this file against a fresh conversion. -->",
      """<graphml xmlns="http://graphml.graphdrawing.org/xmlns" xmlns:java="http://www.yworks.com/xml/yfiles-common/1.0/java" """ +
        """xmlns:sys="http://www.yworks.com/xml/yfiles-common/markup/primitives/2.0" xmlns:x="http://www.yworks.com/xml/yfiles-common/markup/2.0" """ +
        """xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:y="http://www.yworks.com/xml/graphml" xmlns:yed="http://www.yworks.com/xml/yed/3" """ +
        """xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd">""",
      """  <key for="node" id="d0" yfiles.type="nodegraphics"/>""",
      """  <key attr.name="url" attr.type="string" for="node" id="d1"/>""",
      """  <key attr.name="description" attr.type="string" for="node" id="d2"/>""",
      """  <key for="edge" id="d3" yfiles.type="edgegraphics"/>""",
      """  <graph edgedefault="directed" id="G">""") ++ out.result() ++
      Vector("  </graph>", "</graphml>")).mkString("", "\n", "\n")

  /** Every refusal of `source`, and whether `target` (and, when given, the yEd copy) still matches
   *  its conversion.
   *
   *  @param source the `.puml` diagram
   *  @param target the committed GraphML
   *  @param yed    the committed yEd GraphML, when it is to be checked too (F_0098)
   *  @return one finding per problem, empty when the diagram reads and the copies are current
   */
  def offences(source: Path, target: Path, yed: Option[Path] = None): Vector[String] =
    read(Files.read(source)) match
      case Left(errors) => errors.map(e => s"$source: $e")
      case Right(d) =>
        def current(path: Path, fresh: String): Vector[String] =
          if !JFiles.exists(path) then Vector(s"$path: missing; run `sbt graphml`")
          else if Files.read(path).replace("\r\n", "\n") != fresh then
            Vector(s"$path: stale, it no longer matches $source; run `sbt graphml`")
          else Vector.empty
        current(target, toGraphML(d)) ++ yed.toVector.flatMap(current(_, toYEd(d)))

  /** `--check`: report the refusals and a stale copy. Otherwise write the GraphML.
   *
   *  @param args `--check` and/or the source and target paths
   *  @return 1 on any refusal or a stale copy, 0 otherwise
   */
  def run(args: Seq[String]): Int =
    val check = args.contains("--check")
    val paths = args.filterNot(_ == "--check").map(Paths.get(_))
    val (source, target) = (paths.headOption.getOrElse(DefaultSource), paths.lift(1).getOrElse(DefaultTarget))
    val yed = paths.lift(2).getOrElse(DefaultYEdTarget)
    if check then
      Files.report(offences(source, target, Some(yed)), n => s"\n$source: $n diagram problem(s)",
        "The diagram reader refuses what it does not know (F_0097); after editing the diagram, run `sbt graphml`.")
    else
      read(Files.read(source)) match
        case Left(errors) =>
          Files.report(errors.map(e => s"$source: $e"), n => s"\n$source: $n diagram problem(s), nothing written")
        case Right(d) =>
          Files.write(target, toGraphML(d))
          Files.write(yed, toYEd(d))
          println(s"$target: ${d.nodes.size} nodes, ${d.edges.size} edges; $yed for yEd")
          0
