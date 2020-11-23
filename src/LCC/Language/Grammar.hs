module LCC.Language.Grammar (
  LExpr(..)
) where

import LCC.Language.Types ( Binding )
import LCC.Utils.Text ( surroundWithParens )

data LExpr = Var Binding | Abstraction Binding LExpr | Application LExpr LExpr

instance Show LExpr where
  show (Var c) = [c]
  show (Abstraction b e) = surroundWithParens $ "|" <> (b : ".") <> show e
  show (Application l1 l2) = surroundWithParens $ show l1 <> " " <> show l2