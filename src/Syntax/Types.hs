module Syntax.Types ( Type(..) ) where

import Syntax.Common ( Id )

data Type = IntT
          | BoolT
          | CharT
          | Prod Type Type
          | List [Type]
          | Void
          | Fun [Type] Type
          | TyVar Id
  deriving (Show, Eq)
