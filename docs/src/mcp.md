---
title: MCP server
nav_order: 18
---

# Leonardo as a tool for an AI assistant (MCP)

The **Model Context Protocol** (MCP) is how an LLM client — Claude Desktop, Claude Code, and other
assistants — calls external tools. `leonardo-mcp` is a server that gives such a client Leonardo's
REPL: the assistant delegates symbolic and exact or certified numeric work to it instead of
computing in-model, where language models are unreliable. Every answer is Leonardo's own: when it
cannot compute something it says so with a note, rather than guessing.

It needs **JDK 25** (as every Leonardo module does since this release) and nothing else: no Python,
no Mathematica.

## Installing

Each [GitHub Release](https://github.com/lycogrypho/Leonardo/releases) carries a runnable jar,
`leonardo-mcp-@VERSION@.jar`. Run it over stdio:

```bash
java --sun-misc-unsafe-memory-access=allow -jar leonardo-mcp-@VERSION@.jar
```

Or straight from Maven Central with [coursier](https://get-coursier.io):

```bash
cs launch it.grypho:leonardo-mcp_3:@VERSION@ --java-opt --sun-misc-unsafe-memory-access=allow
```

The `--sun-misc-unsafe-memory-access=allow` option only silences a warning JDK 25 prints about the
Scala 3.3 runtime; the server works the same without it. `--timeout <seconds>` (default 30) bounds
one request.

## Client configuration

A client starts the server as a command. For Claude Desktop, in `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "leonardo": {
      "command": "java",
      "args": ["--sun-misc-unsafe-memory-access=allow", "-jar", "/path/to/leonardo-mcp-@VERSION@.jar"]
    }
  }
}
```

For Claude Code: `claude mcp add leonardo -- java --sun-misc-unsafe-memory-access=allow -jar /path/to/leonardo-mcp-@VERSION@.jar`.
The `java` must be a JDK 25; give its full path if an older one comes first on the `PATH`.

## What the server offers

**Tools.** `evaluate(line)` runs one line of the [REPL](repl.md) on the connection's session, so
assignments (`f := sin(x) + x`) are kept for later lines and every feature is reachable. Typed tools
cover the commonest operations with checked arguments: `simplify`, `derive`, `integrate` (indefinite,
or definite with both bounds), `solve` (which, unlike a bare `solve` line, does not bind the variable)
and `limit`. `scala_value(line)` returns a line's result as Scala source constructing it, to paste into
a program that depends on the library.

**Resources.** `leonardo://grammar` is the [cheat sheet](cheatsheet.md), the whole grammar with
examples, for the assistant to read before writing a line; `leonardo://session/script` is the
session's state as a replayable script, what `:save` writes.

**Answers.** The text is the REPL's own answer, notes included. The `names` diagnostics are on, so an
unknown call such as `sqrt(2)` (the grammar has no `sqrt`; it is `2^(1/2)`) is reported rather than
read as a product, and a first use of a name is announced. When the answer carries no note, a second
part gives its LaTeX.

**Limits.** Every request runs under the library's work budgets and a wall-clock timeout. A request
that times out is answered at once with an error and the session continues from its state before
that request; the abandoned computation runs on in the background until its budget stops it, since a
computation cannot be interrupted.  While four abandoned computations are still running, a new
request is refused at once with that reason and accepted again as soon as one finishes; if restoring
the session itself does not finish within the timeout, the session starts empty, and the error says so.
Requests are answered one at a time; one that arrives while another is being answered waits its turn,
and its limit counts from its arrival, so if the limit passes while it waits it is refused without
being started.
