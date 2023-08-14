# SPL Compiler

Current parser testing setup:
```
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Void
:set -XOverloadedStrings
parseTest <parser> <input string>
```

**To do:**
- [ ] Printer helpers for parentheses, brackets and braces
- [ ] Test type checking and type generation
- [ ] Code generation
- [ ] Extension
- [ ] Improve parser errors


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
