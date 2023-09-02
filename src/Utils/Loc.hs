module Utils.Loc (
  Loc(..),
  defaultLoc,
  HasLoc(..)
) where

import Text.Megaparsec.Pos (SourcePos(..), mkPos)


-- | Source code locations with start and end position
data Loc = Loc !SourcePos !SourcePos
  deriving (Eq,Ord)

instance Show Loc where
  show (Loc _ _) = "<loc>"

defaultLoc :: Loc
defaultLoc = Loc (SourcePos "DEFAULT" (mkPos 1) (mkPos 1))
                 (SourcePos "DEFAULT" (mkPos 1) (mkPos 1))

-- | Typeclass for data featuring a source code location
class HasLoc a where
  getLoc :: a -> Loc

  getStart :: a -> SourcePos
  getStart x = (\(Loc start _) -> start) $ getLoc x

  getEnd :: a -> SourcePos
  getEnd x = (\(Loc _ end) -> end) $ getLoc x
