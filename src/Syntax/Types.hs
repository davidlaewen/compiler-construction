module Syntax.Types ( Type(..) ) where

import Syntax.Common ( Id )

data Type = Int
          | Bool
          | Char
          | Prod Type Type
          | List Type
          | Void
          | Fun [Type] Type
          | TyVar Id
  deriving (Show, Eq)
