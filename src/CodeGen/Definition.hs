module CodeGen.Definition (
  concatMapM,
  uTypeSize
) where

import TypeInference.Definition (UType)
import qualified TypeInference.Definition as UType

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
