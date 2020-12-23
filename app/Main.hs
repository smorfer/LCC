module Main where

import           LCC.Parsing.Parsing
import           LCC (lccparseTest)

main :: IO ()
main = do
  --putStrLn "LCC general testing"
  --lccTest
  --putStrLn "Parsing testing"
  --parsingTest
  putStrLn "LCC file read and parse"
  lccparseTest
