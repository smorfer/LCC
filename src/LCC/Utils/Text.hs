module LCC.Utils.Text (
  surroundWith,
  surroundWithParens,
lambdaSymbol) where

lambdaSymbol :: String
lambdaSymbol = "\120698"

surroundWith :: Char -> Char -> String -> String
surroundWith lhs rhs s =  lhs : s <> [rhs]

surroundWithParens :: String -> String
surroundWithParens = surroundWith '(' ')'
