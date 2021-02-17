module LCC.Language.Reduction (betaApply) where

import           LCC.Language.Grammar (isReducible, LExpr(..))
import           LCC.Language.Types (Binding)
import           LCC.Language.Transformation

{-
- I need to figure out the steps for every reduction necessary
- Let there be a lambda term A
- A reducible term consists of an application with the lhs(left-hand side)  being an abstraction
- Every variable in the lhs needs to be collected and checked against every variable in the rhs(right-hand side)
- Then every variable in the rhs also used in the lhs needs to be replaced with an unused variable binding(letter)
-}
-- mBetaReduction :: Binding -> LExpr -> LExpr -> ()
betaReduction :: Binding -> LExpr -> LExpr -> LExpr
betaReduction b (Var v) rhs = if b == v
                              then rhs
                              else Var v
betaReduction b (Abstraction b' e) rhs
  | b == b = error $ "Duplicate Binding: " ++ [b]
  | otherwise = Abstraction b' $ betaReduction b e rhs
betaReduction b (Curried bs e) rhs
  | b `elem` bs = error $ "Duplicate Binding: " ++ [b]
  | otherwise = Curried bs $ betaReduction b e rhs
betaReduction b (Application lhs rhs') rhs =
  Application (betaReduction b lhs rhs) (betaReduction b rhs' rhs)

betaApply :: LExpr -> LExpr
betaApply = until (not . isReducible) (reduce . deduplicateBindings)

reduce :: LExpr -> LExpr
reduce (Application (Abstraction b e) rhs) = betaReduction b e rhs
reduce (Application (Curried (b:[b']) e) rhs) =
  Abstraction b' $ betaReduction b e rhs
reduce (Application (Curried (b:bs) e) rhs) =
  Curried bs $ betaReduction b e rhs
reduce (Application lhs rhs) = Application (reduce lhs) (reduce rhs)
reduce (Abstraction b e) = Abstraction b $ reduce e
reduce (Curried bs e) = Curried bs $ reduce e
reduce e = e

