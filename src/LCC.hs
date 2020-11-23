module LCC
    ( lccTest
    ) where

import LCC.Language.Grammar
import LCC.Language.Reduction


lccTest :: IO ()
lccTest = do
  let testExpr = Application (Abstraction 'x' (Var 'x')) (Var 'y')
      reduced = betaReduction' testExpr
  putStrLn "LCC WIP"
  print testExpr
  putStrLn "Reducing..."
  print reduced

