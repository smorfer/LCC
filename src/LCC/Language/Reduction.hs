module LCC.Language.Reduction (betaReduction, betaReduction', betaApply) where

import           LCC.Language.Grammar (LExpr(..))
import           LCC.Language.Types (Binding)

betaReduction :: LExpr -> LExpr -> LExpr
betaReduction subject substitute = case subject of
  v@(Var _) -> Application v substitute
  (Abstraction b e) -> betaReduceAbstraction b e substitute
  (Curried (b:bs) e) -> case bs of
    [] -> betaReduceAbstraction b e substitute
    _  -> Curried bs $ betaReduceAbstraction b e substitute
  (Application v@(Var _) e) -> Application v e
  (Application lhs rhs) -> betaReduction (betaReduction lhs rhs) substitute
  where
    betaReduceAbstraction :: Binding -> LExpr -> LExpr -> LExpr
    betaReduceAbstraction c exp sub = case exp of
      inner_v@(Var e_c) -> if e_c == c
                           then sub
                           else inner_v
      (Abstraction inner_b inner_e)
        -> Abstraction inner_b (betaReduceAbstraction c inner_e sub)
      (Application inner_lhs inner_rhs) -> betaReduction
        (betaReduceAbstraction c inner_lhs sub)
        (betaReduceAbstraction c inner_rhs sub)
      (Curried inner_bs inner_e)
        -> Curried inner_bs $ betaReduceAbstraction c inner_e sub

-- reduction of an application
betaReduction' :: LExpr -> LExpr
betaReduction' (Application lhs rhs) = betaReduction lhs rhs
betaReduction' (Abstraction b exp) = Abstraction b $ betaReduction' exp
betaReduction' (Curried bs exp) = Curried bs $ betaReduction' exp
betaReduction' (Const c) = error $ "Const " ++ c ++ " in betaReduction"
betaReduction' e = e

betaApply :: LExpr -> LExpr
betaApply = betaReduction'
