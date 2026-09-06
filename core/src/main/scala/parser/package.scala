package it.grypho.scala.leonardo

/** String-to-AST parser for the Leonardo expression language.
 *
 *  The single public entry point is `Parser.parse(str)`, which returns a
 *  `ParseResult[_Expression]`.  The grammar covers:
 *
 *  - Arithmetic expressions with the full operator precedence
 *    (`^` > unary `+`/`-` > `*`/`/` > `+`/`-`), right-associative `^`,
 *    and implicit multiplication (`3sin(a)`, `3theta`).
 *  - All scalar functions (`sin`, `cos`, `exp`, `ln`, `log`, …) and
 *    functionals (`derive`, `integral`, `solve`, `solveSystem`, `limit`,
 *    `laplace`, `fourier`, `invlaplace`, `ode`).
 *  - Matrix literals (`[[a, b], [c, d]]`) and matrix operations
 *    (`transpose`, `det`, `inv`, `eye`, `zeros`, `lu`, `qr`, `eigen`,
 *    `eig`, `jordan`).
 *  - Equation relations (`lhs = rhs` -> `_Equation`) and equality checks
 *    (`lhs == rhs` -> `_EqualityCheck`).
 *  - The imaginary unit `i`, the constants `pi` and `e`, and scientific
 *    notation (`3E-5`).
 *
 *  Package layering: `parser` imports every domain package (`core`, `scalar`,
 *  `matrix`, `equation`, `transform`, `ode`) and is itself imported only by
 *  the `cli` leaf package.
 */
package parser
