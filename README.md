# SPL Compiler

## To Do
Code gen:
- [ ] Global variables
- [ ] Tuples
- [ ] Lists
- [ ] Polymorphism

Extension:
- [ ] Lambdas
- [ ] Further idea

Pretty printing:
- [ ] Printer helpers for parentheses, brackets and braces
- [ ] Possibly refactor with `Prettyprinter` library

QoL:
- [ ] Improve parser errors
- [ ] Locations for all type checking errors
- [ ] Better type checking errors
- [ ] Constant folding

Testing & Debugging:
- [ ] Implement test suite
- [ ] Set up workflow/action for testing
- [ ] Test type checking and type generation


## Test inputs
```
scale(p, scalar) :: (Int, Int) Int -> (Int, Int) {\n	return (p.fst * scalar, p.snd * scalar);\n}

foo (n) :: Int -> (Int, Int) \n { 	return (2, 2);\n }\n transpose (p1, p2) :: (Int, Int) (Int, Int) -> (Int, Int) \n { 	return ((p1.fst + p2.fst), (p1.snd + p2.snd));\n}\n\nscale(p, scalar) :: (Int, Int) Int -> (Int, Int) {\n	return (p.fst * \n scalar, p.snd * scalar);}

/*\n    Three ways to implement the factorial function in SPL.\n    First the recursive version.\n*/\nfacR ( n ) :: Int -> Int {\n    if ( n < 2 ) {\n        return 1;\n    } else {\n        return n * facR ( n - 1 );\n    }\n}



var x = 1

+ foo(x) = x;
```

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
