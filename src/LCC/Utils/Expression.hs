module LCC.Utils.Expression
    ( getBindings
    , getFreeBindings
    , uniqueBindings) where

import           LCC.Language.Types
import           LCC.Language.Grammar
import           LCC.Utils.Set
import           Data.Set

getBindings :: LExpr -> [Binding]
getBindings (Var b) = [b]
getBindings (Const _) = []
getBindings (Application lhs rhs) = getBindings lhs ++ getBindings rhs
getBindings (Abstraction b exp) = b:getBindings exp
getBindings (Curried bs exp) = bs ++ getBindings exp

uniqueBindings :: LExpr -> Set Binding
uniqueBindings = fromList . getBindings

getFreeBindings :: LExpr -> LExpr -> [Binding]
getFreeBindings main sub =
  let allBindings = allowedBindings
      mainBindings = uniqueBindings main
      subBindings = uniqueBindings sub
  in reverse
     $ toList
     $ allBindings
     `intersection` symmetricDifference
       allBindings
       (mainBindings `union` subBindings)
