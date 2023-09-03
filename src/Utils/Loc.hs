module Utils.Loc (
  Loc(..),
  defaultLoc,
  HasLoc(..)
) where

import Text.Megaparsec.Pos (SourcePos(..), sourcePosPretty, pos1, unPos)


-- | Source code locations with start and end position
data Loc = Loc !SourcePos !SourcePos
  deriving (Eq,Ord)

instance Show Loc where
  show (Loc start end) = sourcePosPretty start <> "-" <>
    (if startLine == endLine then "" else show endLine <> ":") <> show endCol
    where
      startLine = unPos $ sourceLine start
      endLine = unPos $ sourceLine end
      endCol = unPos $ sourceColumn end


defaultLoc :: Loc
defaultLoc = Loc defaultPos defaultPos
  where defaultPos = SourcePos "DEFAULT" pos1 pos1

-- | Typeclass for data featuring a source code location
class HasLoc a where
  getLoc :: a -> Loc

  getStart :: a -> SourcePos
  getStart x = (\(Loc start _) -> start) $ getLoc x

  getEnd :: a -> SourcePos
  getEnd x = (\(Loc _ end) -> end) $ getLoc x
