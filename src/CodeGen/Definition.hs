{-# LANGUAGE OverloadedStrings, OverloadedRecordDot #-}

module CodeGen.Definition (
  SSMProgram,
  LocationMap, Location(..),
  CtorData(..), CtorMap, SelectorMap,
  nullPtr,
  CodegenState(..), Codegen, runCodegen,
  freshLabel, modifyOffsets, modifyHeapLocs,
  insertCtorData, insertSelector,
  concatMapM,
  uTypeSize
) where

import CodeGen.Instructions (Instr(..))
import TypeInference.Types (UScheme, UType)
import qualified TypeInference.Types as TI (UScheme(..), UType(..))

import Control.Monad.State (State, evalState, modify, gets)
import qualified Data.Map as M
import qualified Data.Text as T


type SSMProgram = [Instr]

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

insertSelector :: T.Text -> Int -> Codegen ()
insertSelector name offset = modify (\s ->
  s { selectorMap = M.insert name offset s.selectorMap })

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM _ [] = pure []
concatMapM f (x : xs) = do
  y <- f x
  ys <- concatMapM f xs
  pure $ y ++ ys

uSchemeSize :: UScheme -> Int
uSchemeSize (TI.UScheme _ ty) = uTypeSize ty

uTypeSize :: UType -> Int
uTypeSize TI.Int = 1
uTypeSize TI.Bool = 1
uTypeSize TI.Char = 1
uTypeSize (TI.Prod _ _) = 1
uTypeSize (TI.List _) = 1
uTypeSize (TI.UVar _) = 1
uTypeSize t = error $ "Called uTypeSize on illegal type: " <> show t

uTypeSizeHeap :: UType -> Int
uTypeSizeHeap (TI.List ty) = uTypeSize ty + 1
uTypeSizeHeap (TI.Prod ty1 ty2) = uTypeSize ty1 + uTypeSize ty2
uTypeSizeHeap ty = uTypeSize ty
