# SPL Compiler

## To Do
Code gen:
- [x] Global variables
- [ ] Tuples
- [ ] Lists
- [ ] Compute `heapLow` dynamically
- [ ] Polymorphism

Extension:
- [ ] Lambdas
- [ ] Further idea

Pretty printing:
- [ ] Printer helpers for parentheses, brackets and braces
- [ ] Possibly refactor with `Prettyprinter` library

QoL:
- [ ] Tarjan's algorithm for fun decl ordering and grouping for mutual recursion
- [ ] Automatic insertion of `return` statements
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

[SPL test files](https://docs.haskellstack.org/en/stable/GUIDE/)

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
