module LCC.Language.Reduction (
  alphaConversion,
  betaReduction,
  betaReduction'
) where

import LCC.Language.Grammar ( LExpr(..) )
import LCC.Language.Types (Binding)

alphaConversion :: Binding -> LExpr -> Binding -> LExpr
alphaConversion old exp new = case exp of 
  v@(Var c) -> if c == old then Var new else v
  (Abstraction b e) -> if b == old then Abstraction new (alphaConversion old e new) else Abstraction b (alphaConversion old e new)
  (Application l1 l2) -> Application (alphaConversion old l1 new) (alphaConversion old l2 new)

betaReduction :: LExpr -> LExpr -> LExpr
betaReduction subject substitute = 
  case subject of
    v@(Var _) -> Application v substitute
    (Abstraction b e) -> betaReduceAbstraction b e substitute
    (Application l1 l2) -> betaReduction (betaReduction l1 l2) substitute
  where
    betaReduceAbstraction :: Binding -> LExpr -> LExpr -> LExpr
    betaReduceAbstraction c exp sub =
      case exp of
        inner_v@(Var e_c) -> if e_c == c then sub else inner_v
        (Abstraction inner_b inner_e) -> Abstraction inner_b (betaReduceAbstraction c inner_e sub)
        (Application inner_l1 inner_l2) -> betaReduction (betaReduceAbstraction c inner_l1 sub) (betaReduceAbstraction c inner_l2 sub)


-- reduction of an application
betaReduction' :: LExpr -> LExpr
betaReduction' (Application l1 l2) = betaReduction l1 l2
betaReduction' e = e