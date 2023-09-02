{-# LANGUAGE OverloadedStrings #-}

module CodeGen.Definition (
  LocationMap,
  Program,
  Location(..),
  nullPtr,
  CodegenState(..),
  Codegen,
  runCodegen,
  freshLabel,
  modifyOffsets,
  modifyHeapLocs,
  concatMapM,
  uSchemeSize,
  uTypeSize,
  uTypeSizeHeap
) where

import CodeGen.Instructions (Instr(..))
import TypeInference.Definition (UScheme,UType)
import qualified TypeInference.Definition as TI

import Control.Monad.State (State, evalState, modify, gets)
import qualified Data.Map as M
import qualified Data.Text as T


type LocationMap = M.Map T.Text Int
type Program = [Instr]

data Location = Offset Int | HeapLoc Int

nullPtr :: Int
nullPtr = 0xF0F0F0F0

data CodegenState = CodegenState {
  labelCounter :: Int,
  offsets :: LocationMap,
  heapLocs :: LocationMap
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

modifyOffsets :: (LocationMap -> LocationMap) -> Codegen ()
modifyOffsets f = modify (\s -> s { offsets = f (offsets s) })

modifyHeapLocs :: (M.Map T.Text Int -> M.Map T.Text Int) -> Codegen ()
modifyHeapLocs f = modify (\s -> s { heapLocs = f (heapLocs s) })

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
