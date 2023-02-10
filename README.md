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

```
var x = 1

+ foo(x) = x;
```

## Useful docs & tutorials
[Haskell Tool Stack](https://docs.haskellstack.org/en/stable/GUIDE/)
[SPL test files](https://docs.haskellstack.org/en/stable/GUIDE/)
[Megaparsec tutorial blogpost](https://markkarpov.com/tutorial/megaparsec.html)
[Duo Lang parser module](https://github.com/duo-lang/duo-lang/tree/main/src/Parser)
