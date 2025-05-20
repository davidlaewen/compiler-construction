{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Parser.Definition (TokenStream)
import Parser.Lexer (lexer)
import Parser.Parser (parser)
import qualified Syntax.ParseAST as ParseAST (Program)
import qualified Syntax.TypeAST as TypeAST (Program(..))

import Pretty.Parsing (prettyPrinter)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty)
import System.IO (hPutStrLn, stderr)
import Control.Exception (try, IOException)
import Syntax.Desugar (desugar)
import TypeInference.Definition (UType, UScheme, runCGen)
import TypeInference.ConstraintGen (checkProgram)
import TypeInference.Annotate (annotateProgram)
import CodeGen.CodeGen (codegen, runCodegen)
import Pretty.Typing (printProgram)
import CallGraph.Definition (runGraphGen, findSCCs, programFromSCCs)
import CallGraph.GraphGen (programToGraph)

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

printStage :: Show a => Stage a (IO ())
printStage = Stage $ \_ input -> Right $ print input

parsePrintStage :: Stage ParseAST.Program (IO ())
parsePrintStage = Stage $ const $ \program -> Right $ prettyPrinter program

desugarStage :: Stage ParseAST.Program (TypeAST.Program () ())
desugarStage = Stage $ \_ p -> Right $ desugar p

sccStage :: Stage (TypeAST.Program () ()) (TypeAST.Program () ())
sccStage = Stage $ \_ p@(TypeAST.Program varDecls _) ->
  case runGraphGen (programToGraph p) of
    Left err -> Left $ hPutStrLn stderr $ T.unpack err
    Right graphGenState -> Right $ programFromSCCs varDecls (findSCCs graphGenState)

typecheckStage :: Stage (TypeAST.Program () ()) (TypeAST.Program UType UScheme)
typecheckStage = Stage $ \_ p ->
  case runCGen (checkProgram p) of
    Left err -> Left $ hPutStrLn stderr $ T.unpack err
    Right (p', s) -> Right $ annotateProgram s p'

typePrintStage :: Stage (TypeAST.Program UType UScheme) (IO ())
typePrintStage = Stage $ const $ \program -> Right $ printProgram program

codeGenStage :: Stage (TypeAST.Program UType UScheme) String
codeGenStage = Stage $ \_ p -> Right $ unlines $ show <$> runCodegen (codegen p)

putStrStage :: Stage String (IO ())
putStrStage = Stage $ \_ str -> Right $ putStr str

data Args = Args FilePath (Stage T.Text (IO ()))

parseArgs :: [String] -> Maybe Args
parseArgs ("lex" : filePath : _) =
  Just (Args filePath (lexStage >-> printStage))
parseArgs ("parse" : filePath : _) =
  Just (Args filePath (lexStage >-> parseStage >-> printStage))
parseArgs ("prettyprint" : filePath : _) =
  Just (Args filePath (lexStage >-> parseStage >-> parsePrintStage))
parseArgs ("desugar" : filePath : _) =
  Just (Args filePath (lexStage >-> parseStage >-> desugarStage >-> printStage))
parseArgs ("sccs" : filePath : _) =
  Just (Args filePath (lexStage >-> parseStage >-> desugarStage >-> sccStage >-> printStage))
parseArgs ("typecheck" : filePath : _) =
  Just (Args filePath (lexStage >-> parseStage >-> desugarStage >-> sccStage >-> typecheckStage >-> printStage))
parseArgs ("typeprint" : filePath : _) =
  Just (Args filePath (lexStage >-> parseStage >-> desugarStage >-> sccStage >-> typecheckStage >-> typePrintStage))
parseArgs ("codegen" : filePath : _) =
  Just (Args filePath (lexStage >-> parseStage >-> desugarStage >-> sccStage >-> typecheckStage >-> codeGenStage >-> putStrStage))
parseArgs _ = Nothing

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Nothing -> hPutStrLn stderr
      "Usage:\n\t\
      \spl-compiler lex <filename>\n\t\
      \spl-compiler parse <filename>\n\t\
      \spl-compiler prettyprint <filename>\n\t\
      \spl-compiler desugar <filename>\n\t\
      \spl-compiler sccs <filename>\n\t\
      \spl-compiler typecheck <filename>\n\t\
      \spl-compiler typeprint <filename>\n\t\
      \spl-compiler codegen <filename>"
    Just (Args filePath stage) -> do
      try (T.readFile filePath) >>= loadFile filePath stage

loadFile :: [Char] -> Stage i (IO ()) -> Either IOException i -> IO ()
loadFile filePath _ (Left (_ :: IOException)) = hPutStrLn stderr $ "ERROR: Could not read " ++ filePath
loadFile filePath stage (Right contents) =
  case runStage stage filePath contents of
    Left errorIO -> errorIO >> exitFailure
    Right successIO -> successIO
