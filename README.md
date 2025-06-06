# SPL Compiler

## To Do
Parsing:
- [ ] Prevent function calls in global variable declarations(?)
- [ ] Alternatively, allow function calls in global var decls, but then we need
      to allow them to be interspersed with the function decls, and include them
      in the call graph analysis
- [ ] What if global variables and functions are mutually defined? That is, the
      variable declaration calls a function which refers to the variable in the
      function body.
- [x] Remove `mutual { ... }` syntax, since mutually defined groups are now
      automatically determined by the call graph analysis

Code gen:
- [ ] Equality and printing
- [ ] Prelude with printing and comparison implementations
- [ ] Strip no-op instructions from output (e.g. `ajs 0`)

Extension (one of the following):
- [ ] Anonymous functions (lambda notation)
- [x] Custom algebraic data types (records with field selectors)
- [ ] Custom typeclasses with dictionary passing

Pretty printing:
- [x] Printer helpers for parentheses, brackets and braces
- [ ] Possibly refactor with `Prettyprinter` library
- [x] Add pretty printer for `TypeAST`

QoL:
- [x] Tarjan's algorithm for fun decl ordering and grouping for mutual recursion
- [ ] Automatic insertion of `return` statements for Void functions
- [ ] Improve parser errors
- [x] Location data in AST for use in errors
- [ ] Print code snippets with underlining in error messages
- [ ] Constant folding
- [ ] Parsing of escaped characters
- [ ] Built-in `printLn` function

Testing & Debugging:
- [ ] Implement test suite
- [ ] Set up workflow/action for testing
- [ ] Test type checking and type generation

## Useful docs & tutorials
[Haskell Tool Stack](https://docs.haskellstack.org/en/stable/GUIDE/)

[Hoogle](https://hoogle.haskell.org/)

[SSM instruction overview](https://gitlab.science.ru.nl/compilerconstruction/ssm/-/blob/master/HELP.md)

[Megaparsec tutorial blogpost](https://markkarpov.com/tutorial/megaparsec.html)

[Data.Graph documentation](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Graph.html)

[Duo Lang parser module](https://github.com/duo-lang/duo-lang/tree/main/src/Parser)

## Old parser testing setup
```
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Void
:set -XOverloadedStrings
parseTest <parser> <input string>
```
