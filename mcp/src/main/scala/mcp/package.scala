package it.grypho.scala.leonardo

/** **The MCP server** (F_0126): Leonardo as a tool an LLM client calls through the Model Context
 *  Protocol, over one REPL `cli.Session` per connection.
 *
 *  The design is the owner's (`DesignNotes.md` ▸ F_0126): one `evaluate(line)` tool carries every
 *  feature and the session's state, a few typed tools cover the commonest operations with a JSON
 *  schema that catches a wrong argument shape, and the grammar (the cheat sheet) and the session's
 *  `:save` script are served as resources.  `names` is forced on, since an unknown call is otherwise a
 *  silent product, and `latex` on, whose rendering travels as a second content part.  A request that
 *  passes its wall-clock timeout is answered at once and its worker abandoned — nothing can interrupt a
 *  computation, its work budgets eventually stop it — the session continuing on a fresh worker restored
 *  from its script.  The official Java SDK carries the protocol, confined to this module.
 */
package mcp
