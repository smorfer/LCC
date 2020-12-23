module LCC (lccTest, lccparseTest) where

import           Data.Set (fromList)
import           LCC.Language.Grammar (LExpr(Abstraction, Application, Var))
import           LCC.Language.Reduction (betaReduction, betaApply)
import           LCC.Utils.Expression (getBindings, getFreeBindings)
import           LCC.Language.Types ()
import           LCC.Language.Conversion (alphaConversion)
import           LCC.Language.Transformation (prepareMap, substitute, assign
                                            , curryE, deduplicateBindings
                                            , ExprMap)
import           LCC.Parsing.Parsing
import           Text.Megaparsec
import           Data.Either
import qualified Data.Map.Strict as MS
import           Data.Maybe

runLCC :: ExprMap -> [Expression] -> IO ()
runLCC m ((Out e):es) = do
  print $ pipeline m e
  runLCC m es
runLCC m ((Assignment c e):es) = let m' = assign m c $ pipeline m e
                                 in runLCC m' es
runLCC _ [] = putStrLn "FINISH"

pipeline :: ExprMap -> LExpr -> LExpr
pipeline m exp = curryE
  $ betaApply
  $ deduplicateBindings
  $ substitute (prepareMap m exp)
  $ curryE exp

lccparseTest :: IO ()
lccparseTest = do
  let filename = "lcc_src/test.lcc"
      run input = parse parseLCC filename input
  expr <- lines <$> readFile filename
  let parsed = run <$> expr
  if any isLeft parsed
    then do
      putStrLn $ unlines $ errorBundlePretty <$> lefts parsed
    else runLCC MS.empty $ catMaybes $ rights parsed
  return ()

lccTest :: IO ()
lccTest = do
  let identity = Abstraction 'a' (Var 'a')
      zero = Abstraction 's' (Abstraction 'z' (Var 'z'))
      -- λn.λf.λx.f (n f x)
      -- TODO: Incrementing numbers gives rise to the problem of duplicate bindings in two expressions, needs to be fixed, alphaConversion when needed
      increment = Abstraction
        'n'
        (Abstraction
           'f'
           (Abstraction
              'x'
              (Application
                 (Var 'f')
                 (Application (Application (Var 'n') (Var 'f')) (Var 'x')))))
      one = Application increment zero
      reducedone = betaApply one
      increment' = alphaConversion 'n' 'm'
        $ alphaConversion 'x' 'y'
        $ alphaConversion 'f' 'g' increment
      two = Application increment' reducedone
      reducedtwo = betaApply two
      three = Application increment reducedtwo
      threeBindings = getBindings three
      reducedthree = betaApply three
  putStrLn "LCC WIP"
  print zero
  print one
  putStrLn "Reducing ..."
  print reducedone
  print two
  putStrLn "Reducing..."
  print reducedtwo
  print reducedthree
  print $ curryE increment
  print $ getFreeBindings increment reducedthree
  let exp = curryE increment
    in do
         print
           $ deduplicateBindings
           $ Application exp (Application exp (Application exp one))
         print
           $ betaApply
           $ deduplicateBindings
           $ Application exp (Application exp (Application exp one))
         print $ alphaConversion 'f' 'z' $ alphaConversion 'x' 'y' exp
