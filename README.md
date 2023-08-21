# SPL Compiler

## To Do
Code gen:
- [x] Global variables
- [x] Tuples
- [ ] Lists
- [ ] Polymorphism
- [ ] Equality and printing
- [ ] Support for mutual recursion
- [ ] Compute `heapLow` dynamically
- [ ] Strip output of no-op instructions (e.g. `ajs 0`)
- [ ] Prelude with printing and comparison implementations

Extension:
- [ ] Lambdas
- [ ] Further idea

Pretty printing:
- [ ] Printer helpers for parentheses, brackets and braces
- [ ] Possibly refactor with `Prettyprinter` library

QoL:
- [ ] Tarjan's algorithm for fun decl ordering and grouping for mutual recursion
- [ ] Automatic insertion of `return` statements for Void functions
- [ ] Improve parser errors
- [ ] Locations for all type checking errors
- [ ] Better type checking errors
- [ ] Constant folding
- [ ] Parsing of non-ANSI Unicode characters

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
