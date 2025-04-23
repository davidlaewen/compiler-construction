module CallGraph.Definition () where

import Data.Graph
import qualified Syntax.TypeAST as T

programToGraph :: T.Program () () -> Graph
programToGraph p = buildG (0,0) []
