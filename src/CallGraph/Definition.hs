{-# LANGUAGE LambdaCase, OverloadedRecordDot #-}

module CallGraph.Definition (
  GraphGen, runGraphGen, insertDecl, nameToVertex, insertEdge,
  buildCallGraph, findSCCs, programFromSCCs
) where

import Data.Graph
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Text as T

import Syntax.TypeAST
import qualified Data.Set as S
import Data.Functor ((<&>))
import Utils.Loc (defaultLoc)



type Name = T.Text
type NameMap = M.Map Name Vertex
type DeclMap = M.Map Vertex (FunDecl () ())
type EdgeMap = M.Map Vertex (S.Set Vertex)
type VertexBound = Vertex

-- The input type of `stronglyConnComp` is `[(node, key, [key])]`, where we
-- can obtain the keys from the domain of `edgeMap`/`declMap`, the nodes from
-- `declMap`, and the key lists from `edgeMap` with `S.toList`.

-- TODO: Error localisation (calls to undefined functions)

-- Data structures
data GraphGenState = GraphGenState {
  nameMap :: NameMap,
  declMap :: DeclMap,
  edgeMap :: EdgeMap,
  vtxBound :: VertexBound
}

-- Call graph generation monad
type GraphGen = StateT GraphGenState (Except T.Text)

runGraphGen :: GraphGen a -> Either T.Text GraphGenState
runGraphGen x = snd <$> runExcept (runStateT x $
  GraphGenState M.empty M.empty M.empty 0)


modifyNameMap :: (NameMap -> NameMap) -> GraphGen ()
modifyNameMap f = modify (\s -> s{ nameMap = f s.nameMap })

modifyEdgeMap :: (EdgeMap -> EdgeMap) -> GraphGen ()
modifyEdgeMap f = modify (\s -> s{ edgeMap = f s.edgeMap })

modifyDeclMap :: (DeclMap -> DeclMap) -> GraphGen ()
modifyDeclMap f = modify (\s -> s{ declMap = f s.declMap })

insertName :: Name -> Vertex -> GraphGen ()
insertName name ty = modifyNameMap (M.insert name ty)

lookupName :: Name -> GraphGen (Maybe Vertex)
lookupName name = gets (M.lookup name . nameMap)

insertDecl :: Vertex -> FunDecl () () -> GraphGen ()
insertDecl vtx decl = modifyDeclMap (M.insert vtx decl)

freshVertex :: GraphGen Vertex
freshVertex = do
  i <- gets vtxBound
  modify (\s -> s{ vtxBound = i+1 })
  pure i

nameToVertex :: Name -> GraphGen Vertex
nameToVertex name = do
  lookupName name >>= \case
    Just vtx -> pure vtx
    Nothing -> do -- Get fresh vertex and update `nameMap`
      vtx <- freshVertex
      insertName name vtx
      modifyEdgeMap (M.insert vtx S.empty)
      pure vtx

-- | Insert edge from vertex for `f` to node with name `g`
insertEdge :: Vertex -> Name -> GraphGen ()
insertEdge f g = do
  vtx <- nameToVertex g
  modifyEdgeMap (M.update (Just . S.insert vtx) f)

buildCallGraph :: GraphGenState -> [(FunDecl () (), Vertex, [Vertex])]
buildCallGraph s = M.toList s.declMap <&> (\(vtx,decl) ->
  (decl, vtx, S.toList . S.unions $ M.lookup vtx s.edgeMap))

findSCCs :: GraphGenState -> [SCC (FunDecl () ())]
findSCCs s = stronglyConnComp (buildCallGraph s)

funMutDeclFromSCC :: SCC (FunDecl () ()) -> FunMutDecl () ()
funMutDeclFromSCC (AcyclicSCC funDecl) = SingleDecl funDecl
funMutDeclFromSCC (CyclicSCC [funDecl]) = SingleDecl funDecl -- Self-reference is not mutual
funMutDeclFromSCC (CyclicSCC funDecls) = MutualDecls defaultLoc funDecls

programFromSCCs :: [DataDecl] -> [VarDecl ()] -> [SCC (FunDecl () ())] -> Program () ()
programFromSCCs dataDecls varDecls sccs = Program dataDecls varDecls (funMutDeclFromSCC <$> sccs)
