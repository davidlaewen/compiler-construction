{-# LANGUAGE OverloadedStrings, OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}

module CodeGen.Definition (
  SSMProgram,
  LocationMap, Location(..),
  CtorData(..), CtorMap, SelectorMap,
  nullPtr,
  CodegenState(..), Codegen, runCodegen,
  freshLabel, modifyOffsets, modifyHeapLocs,
  insertCtorData, lookupCtorData,
  insertSelector, lookupSelector,
  concatMapM
) where

import CodeGen.Instructions (Instr(..))

import Control.Monad.State (State, evalState, modify, gets)
import qualified Data.DList as D
import qualified Data.Map as M
import qualified Data.Text as T


type SSMProgram = D.DList Instr

-- Mapping of identifiers to stack offsets or heap locations
type LocationMap = M.Map T.Text Int
data Location = Offset Int | HeapLoc Int

-- Constructor information: Names are mapped to labels and field counts
data CtorData = CtorData { label :: Int, fieldCount :: Int }
type CtorMap = M.Map T.Text CtorData
type SelectorMap = M.Map T.Text Int -- Maps selectors to offsets on heap


-- Bit pattern for empty list: out of memory range and easily recognisable
nullPtr :: Int
nullPtr = 0xF0F0F0F0

-- Code generation monad
data CodegenState = CodegenState {
  labelCounter :: Int,
  offsets :: LocationMap,
  heapLocs :: LocationMap,
  ctorMap :: CtorMap,
  selectorMap :: SelectorMap
}

type Codegen = State CodegenState

runCodegen :: Codegen a -> a
runCodegen = flip evalState initialState
  where
    initialState = CodegenState 0 M.empty M.empty M.empty M.empty

-- | Generates a fresh index for ensuring uniqueness of labels
freshLabel :: T.Text -> Codegen T.Text
freshLabel t = do
  i <- gets labelCounter
  modify (\s -> s { labelCounter = i + 1 })
  pure $ t <> "_" <> T.pack (show i)

modifyOffsets :: (LocationMap -> LocationMap) -> Codegen ()
modifyOffsets f = modify (\s -> s { offsets = f s.offsets })

modifyHeapLocs :: (LocationMap -> LocationMap) -> Codegen ()
modifyHeapLocs f = modify (\s -> s { heapLocs = f s.heapLocs })

insertCtorData :: T.Text -> CtorData -> Codegen ()
insertCtorData name cd = modify (\s ->
  s { ctorMap = M.insert name cd s.ctorMap })

lookupCtorData :: T.Text -> Codegen CtorData
lookupCtorData cName = gets (M.lookup cName . ctorMap) >>= \case
  Nothing -> error $ "Couldn't find constructor `" <> T.unpack cName <> "`!"
  Just ctorData -> pure ctorData

insertSelector :: T.Text -> Int -> Codegen ()
insertSelector name offset = modify (\s ->
  s { selectorMap = M.insert name offset s.selectorMap })

lookupSelector :: T.Text -> Codegen Int
lookupSelector name = gets (M.lookup name . selectorMap) >>= \case
  Nothing -> error $ "Couldn't find selector `" <> T.unpack name <> "`!"
  Just offset -> pure offset

concatMapM :: Monad m => (a -> m (D.DList b)) -> [a] -> m (D.DList b)
concatMapM f l = do
  l' <- mapM f l
  pure $ D.concat l'
