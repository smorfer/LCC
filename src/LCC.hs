module LCC (lccparseTest) where

import           Data.Set (fromList)
import           LCC.Language.Grammar (LExpr(Abstraction, Application, Var))
import           LCC.Language.Reduction (betaApply)
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
pipeline m exp =
  curryE $ betaApply $ deduplicateBindings $ substitute m $ curryE exp

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