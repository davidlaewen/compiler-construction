{-# LANGUAGE OverloadedStrings #-}

module CodeGen.Definition (
  LocMap,
  Program,
  Loc(..),
  heapLow,
  CodegenState(..),
  Codegen,
  runCodegen,
  freshLabel,
  modifyOffsets,
  modifyHeapLocs,
  concatMapM,
  uTypeSize
) where

import CodeGen.Instructions (Instr(..))
import TypeInference.Definition (UType)
import qualified TypeInference.Definition as UType

import Control.Monad.State (State, evalState, modify, gets)
import qualified Data.Map as M
import qualified Data.Text as T


type LocMap = M.Map T.Text Int
type Program = [Instr]

data Loc = Offset Int | HeapLoc Int

heapLow :: Int
heapLow = 0x0007D0

data CodegenState = CodegenState {
  labelCounter :: Int,
  offsets :: LocMap,
  heapLocs :: LocMap
}

type Codegen = State CodegenState

runCodegen :: Codegen a -> a
runCodegen = flip evalState initialState
  where
    initialState = CodegenState 0 M.empty M.empty

freshLabel :: T.Text -> Codegen T.Text
freshLabel t = do
  i <- gets labelCounter
  modify (\s -> s { labelCounter = i + 1 })
  pure $ t <> "_" <> T.pack (show i)

modifyOffsets :: (LocMap -> LocMap) -> Codegen ()
modifyOffsets f = modify (\s -> s { offsets = f (offsets s) })

modifyHeapLocs :: (M.Map T.Text Int -> M.Map T.Text Int) -> Codegen ()
modifyHeapLocs f = modify (\s -> s { heapLocs = f (heapLocs s) })

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM _ [] = pure []
concatMapM f (x : xs) = do
  y <- f x
  ys <- concatMapM f xs
  pure $ y ++ ys

uTypeSize :: UType -> Int
uTypeSize UType.Int = 1
uTypeSize UType.Bool = 1
uTypeSize UType.Char = 1
uTypeSize (UType.Prod t1 t2) = uTypeSize t1 + uTypeSize t2
uTypeSize (UType.List _) = 1
uTypeSize t = error $ "Called uTypeSize on illegal type: " <> show t
