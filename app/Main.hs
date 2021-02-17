module Main where

import           LCC.Parsing.Parsing
import           LCC (lccparseTest)
import           GHC.IO.Encoding (utf8, setLocaleEncoding)

main :: IO ()
main = do
  setLocaleEncoding utf8
  --putStrLn "LCC general testing"
  --lccTest
  --putStrLn "Parsing testing"
  --parsingTest
  putStrLn "LCC file read and parse"
  lccparseTest
