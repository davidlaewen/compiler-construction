{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Parser.Definition (TokenStream)
import Parser.Lexer (lexer)
import Parser.Parser (parser)
import qualified Syntax.ParseAST as ParseAST (Program)
import qualified Syntax.TypeAST as TypeAST (Program)

import PrettyPrinter (prettyPrinter)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty)
import System.IO (hPutStrLn, stderr)
import Control.Exception
import Syntax.Desugar (desugar)

newtype Stage i o = Stage {runStage :: FilePath -> i -> Either (IO ()) o}

(>->) :: Stage a b -> Stage b c -> Stage a c
s1 >-> s2 = Stage $ \filePath input -> do
  output <- runStage s1 filePath input
  runStage s2 filePath output

lexStage :: Stage T.Text TokenStream
lexStage = Stage $ \filePath input ->
  case lexer filePath input of
    Left errors -> Left $ hPutStrLn stderr $ errorBundlePretty errors
    Right tokens -> Right tokens

parseStage :: Stage TokenStream ParseAST.Program
parseStage = Stage $ \filePath tokens ->
  case parser filePath tokens of
    Left errors -> Left $ hPutStrLn stderr $ errorBundlePretty errors
    Right program -> Right program

prettyPrintStage :: Stage ParseAST.Program (IO ())
prettyPrintStage = Stage $ const $ \program -> Right $ prettyPrinter program

printStage :: Show a => Stage a (IO ())
printStage = Stage $ \_ input -> Right $ print input

desugarStage :: Stage ParseAST.Program (TypeAST.Program ())
desugarStage = Stage $ \_ p -> Right $ desugar p

data Args = Args FilePath (Stage T.Text (IO ()))

parseArgs :: [String] -> Maybe Args
parseArgs ("lex" : filePath : _) = Just (Args filePath (lexStage >-> printStage))
parseArgs ("parse" : filePath : _) = Just (Args filePath (lexStage >-> parseStage >-> printStage))
parseArgs ("prettyprint" : filePath : _) = Just (Args filePath (lexStage >-> parseStage >-> prettyPrintStage))
parseArgs ("desugar" : filePath : _) = Just (Args filePath (lexStage >-> parseStage >-> desugarStage >-> printStage))
parseArgs _ = Nothing

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Nothing -> hPutStrLn stderr "Usage:\n\tspl-compiler lex <filename>\n\tspl-compiler parse <filename>\n\tspl-compiler prettyprint <filename>"
    Just (Args filePath stage) -> do
      try (T.readFile filePath) >>= loadFile filePath stage

loadFile :: [Char] -> Stage i (IO ()) -> Either IOException i -> IO ()
loadFile filePath _ (Left (_ :: IOException)) = hPutStrLn stderr $ "ERROR: Could not read " ++ filePath
loadFile filePath stage (Right contents) =
  case runStage stage filePath contents of
    Left errorIO -> errorIO >> exitFailure
    Right successIO -> successIO
