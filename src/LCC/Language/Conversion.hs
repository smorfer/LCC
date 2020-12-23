module LCC.Language.Conversion (alphaConversion) where

import           Data.Set
import           LCC.Language.Grammar
import           LCC.Language.Types (Binding)
import           LCC.Utils.Expression

alphaConversion :: Binding -> Binding -> LExpr -> LExpr
alphaConversion old new exp = case exp of
  v@(Var c)           -> if c == old
                         then Var new
                         else v
  (Abstraction b e)   -> if b == old
                         then Abstraction new (alphaConversion old new e)
                         else Abstraction b (alphaConversion old new e)
  (Application l1 l2)
    -> Application (alphaConversion old new l1) (alphaConversion old new l2)
  (Curried bs e)      -> Curried
    ((\b -> if b == old
            then new
            else b)
     <$> bs)
    $ alphaConversion old new e
