module Pretty.Common (
  Indentation, tabWidth, printIndentation,
  putShow,
  sepBy, between, parens, braces, brackets, angles, spaces,
  printBlock
) where

import Data.List (intersperse)
import Parser.Tokens (Symbol (..))

-- Represents the amount of spaces to indent after a newline
type Indentation = Int

tabWidth :: Indentation
tabWidth = 2

printIndentation :: Indentation -> IO ()
printIndentation i = putStr (replicate i ' ')

putShow :: Show a => a -> IO ()
putShow x = putStr $ show x

sepBy :: String -> (a -> IO ()) -> [a] -> IO ()
sepBy sep f xs =
  sequence_ $ intersperse (putStr sep) $ f <$> xs

between :: String -> String -> IO () -> IO ()
between l r x = putStr l >> (x >> putStr r)

parens :: IO () -> IO ()
parens = between (show SymParenLeft) (show SymParenRight)
braces :: IO () -> IO ()
braces = between (show SymBraceLeft) (show SymBraceRight)
brackets :: IO () -> IO ()
brackets = between (show SymBracketLeft) (show SymBracketRight)
angles :: IO () -> IO ()
angles = between (show SymLessThan) (show SymGreaterThan)
spaces :: IO () -> IO ()
spaces = between " " " "

-- Adds a block with braces around printer `p`
printBlock :: Indentation -> IO () -> IO ()
printBlock i p = braces (do
  putChar '\n' >> p >> putChar '\n'
  printIndentation i)
