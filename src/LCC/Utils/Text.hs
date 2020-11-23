module LCC.Utils.Text (
  surroundWith,
  surroundWithParens,
) where

surroundWith :: Char -> Char -> String -> String
surroundWith lhs rhs s =  lhs : s <> [rhs]

surroundWithParens :: String -> String
surroundWithParens = surroundWith '(' ')'
