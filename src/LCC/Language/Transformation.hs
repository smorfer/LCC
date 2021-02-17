module LCC.Language.Transformation
    ( curryE
    , deduplicateBindings
    , assign
    , substitute
    , prepareMap
    , ExprMap) where

import           LCC.Language.Grammar
import           Data.Set (toList, member, Set)
import           LCC.Language.Types
import           LCC.Utils.Expression
import           LCC.Language.Conversion
import           LCC.Utils.Set
import qualified Debug.Trace as Debug
import qualified Data.Map.Strict as MS
import           Data.Maybe
import           GHC.Base (Alternative((<|>)))
import           Data.Either

type ExprMap = MS.Map String LExpr

curryE :: LExpr -> LExpr
curryE v@(Var _) = v
curryE c@(Const _) = c
curryE (Application lhs rhs) = Application (curryE lhs) (curryE rhs)
curryE abst@(Abstraction b exp) = case exp of
  (Abstraction b1 exp1) -> curryE $ Curried [b, b1] exp1
  (Curried bs exp1) -> Curried (b:bs) exp1
  _ -> Abstraction b (curryE exp)
curryE (Curried bs (Abstraction b exp)) = curryE $ Curried (bs ++ [b]) exp
curryE (Curried bs exp) = Curried bs $ curryE exp

uncurryE :: LExpr -> LExpr
uncurryE (Curried (b:bs) exp) = Abstraction b (uncurryE $ Curried bs exp)
uncurryE exp = exp

deduplicateBindings :: LExpr -> LExpr
deduplicateBindings v@(Var _) = v
deduplicateBindings (Const c) = error $ "Const " ++ c ++ " in deduplication"
deduplicateBindings (Abstraction b e) = Abstraction b $ deduplicateBindings e
deduplicateBindings (Curried bs e) = Curried bs $ deduplicateBindings e
deduplicateBindings (Application v@(Var _) e) =
  Application v (deduplicateBindings e)
deduplicateBindings (Application lhs@(Abstraction _ _) rhs) =
  let new_lhs = deduplicateBindings lhs
  in Application new_lhs (dedup new_lhs (deduplicateBindings rhs))
deduplicateBindings (Application lhs@(Curried _ _) rhs) =
  let new_lhs = deduplicateBindings lhs
      uncurried = uncurryE new_lhs
  in Application new_lhs (dedup new_lhs (deduplicateBindings rhs))
deduplicateBindings (Application lhs rhs) =
  let newLhs = deduplicateBindings lhs
      newRhs = deduplicateBindings rhs
  in if isReducible newLhs
     then Application newLhs (dedup newLhs newRhs)
     else Application newLhs newRhs

dedup :: LExpr -> LExpr -> LExpr
dedup lhs rhs = dedup' (uniqueBindings lhs) (getFreeBindings lhs rhs) lhs rhs

dedup' :: Set Binding -> [Binding] -> LExpr -> LExpr -> LExpr
dedup' _ [] _ _ = error
  $ "Deduplication: Binding limit of "
  ++ show (length allowedBindings)
  ++ " exceded"
dedup' used free lhs rhs = foldr
  ($)
  rhs
  (zipWith
     ($)
     (alphaConversion <$> filter (`member` used) (toList $ uniqueBindings rhs))
     free)

assign :: ExprMap -> String -> LExpr -> ExprMap
assign m s exp = MS.insert s exp m

prepareMap :: ExprMap -> LExpr -> ExprMap
prepareMap m e = prepareMap' m e (getConsts e)
  where
    prepareMap' :: ExprMap -> LExpr -> [String] -> ExprMap
    prepareMap' m' exp (c:cs) =
      prepareMap' (MS.update (Just . dedup exp) c m') exp cs
    prepareMap' m' _ [] = m'

getConsts :: LExpr -> [String]
getConsts (Const c) = [c]
getConsts (Var _) = []
getConsts (Application lhs rhs) = getConsts lhs ++ getConsts rhs
getConsts (Abstraction _ e) = getConsts e
getConsts (Curried _ e) = getConsts e

sub :: ExprMap -> LExpr -> Either LExpr LExpr
sub m (Const s) = Right
  $ case MS.lookup s m of
    Just v  -> v
    Nothing -> error $ "Identifier " ++ s ++ " is not declared"
sub m (Application lhs rhs) = case sub m lhs of
  Right e -> Right $ Application e rhs
  Left _  -> case sub m rhs of
    Right e' -> Right $ Application lhs e'
    Left _   -> Left $ Application lhs rhs
sub m (Abstraction b e) = case sub m e of
  Right e' -> Right $ Abstraction b e'
  Left _   -> Left $ Abstraction b e
sub m (Curried bs e) = case sub m e of
  Right e' -> Right $ Curried bs e'
  Left _   -> Left $ Curried bs e
sub m v@(Var _) = Left v

substitute :: ExprMap -> LExpr -> LExpr
substitute m e = either id id
  $ until
    isLeft
    (\exp -> sub (prepareMap m (either id id exp)) (either id id exp))
    (Right e)