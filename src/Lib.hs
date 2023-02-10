module Lib
    ( someFunc,
    lex1,
    lexProgram
    ) where

import Parser.Lexer ( lex1, lexProgram )

someFunc :: IO ()
someFunc = putStrLn "someFunc"
