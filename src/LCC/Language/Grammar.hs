module LCC.Language.Grammar (LExpr(..), allowedBindings, isReducible) where

import           LCC.Language.Types (Binding)
import           LCC.Utils.Text (surroundWithParens, lambdaSymbol)
import           Data.Set

data LExpr = Var Binding
           | Const String
           | Abstraction Binding LExpr
           | Application LExpr LExpr
           | Curried [Binding] LExpr

instance Show LExpr where
  show (Var c) = [c]
  show (Abstraction b e) = surroundWithParens
    (lambdaSymbol <> (b:".") <> show e)
  show (Application l1 l2) = surroundWithParens (show l1 <> " " <> show l2)
  show (Curried (b:bs) e) =
    lambdaSymbol ++ [b] ++ concat ((\a -> [' ', a]) <$> bs) ++ "." ++ show e
  show (Const s) = s

allowedBindings :: Set Binding
allowedBindings = fromList $ ['a' .. 'z'] ++ ['A' .. 'Z']

isReducible :: LExpr -> Bool
isReducible (Application (Abstraction _ _) _) = True
isReducible (Application (Curried _ _) _) = True
isReducible (Application lhs rhs) = isReducible lhs || isReducible rhs
isReducible (Abstraction _ e) = isReducible e
isReducible (Curried _ e) = isReducible e
isReducible _ = False