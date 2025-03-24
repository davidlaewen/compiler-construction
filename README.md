# SPL Compiler

## To Do
Code gen:
- [ ] Equality and printing
- [ ] Prelude with printing and comparison implementations
- [ ] Strip no-op instructions from output (e.g. `ajs 0`)

Extension:
- [ ] Anonymous functions (lambda notation)
- [ ] Custom algebraic data types?
- [ ] Custom typeclasses
- [ ] Further ideas

Pretty printing:
- [ ] Printer helpers for parentheses, brackets and braces
- [ ] Possibly refactor with `Prettyprinter` library
- [ ] Add pretty printer for `TypeAST`

QoL:
- [ ] Tarjan's algorithm for fun decl ordering and grouping for mutual recursion
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

[Megaparsec tutorial blogpost](https://markkarpov.com/tutorial/megaparsec.html)

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
