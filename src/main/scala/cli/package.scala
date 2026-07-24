package it.grypho.scala.leonardo

/** Interactive REPL and syntax-highlighting for the Leonardo CAS.
 *
 *  Two public types live here:
 *
 *  - `Session` — stateful command processor.  `execute(line)` handles one line
 *    of input and returns a `String` result; `script` / `load` serialise and
 *    replay state.  Pure (no IO): the JLine read loop in `repl()` is the only
 *    place that reads the terminal or accesses the filesystem.
 *  - `LeonardoHighlighter` — JLine `Highlighter` that token-colours the prompt
 *    as the user types, using the active `ColorScheme` (`dark`, `light`, `none`).
 *
 *  Package layering: `cli` is the leaf of the dependency graph — it imports
 *  `core`, `scalar`, `matrix`, `equation`, and `parser`, and nothing imports it.
 */
package cli
