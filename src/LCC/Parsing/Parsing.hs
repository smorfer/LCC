module LCC.Parsing.Parsing (parseLCC, parseExpression, Expression(..)) where

import           Text.Megaparsec
import           LCC.Language.Grammar (LExpr(Const, Application, Abstraction, Curried, Var))
import           Data.Void
import           Text.Megaparsec.Char
import           LCC.Utils.Text
import           Control.Monad
import           Data.List.NonEmpty

type Parser = Parsec Void String

data Expression = Assignment String LExpr
                | Out LExpr
  deriving (Show)

bindingSymbols :: [String]
bindingSymbols = [lambdaSymbol, "|"]

parseIdentifier :: Parser String
parseIdentifier = some upperChar

pSpace :: Parser p -> Parser p
pSpace f = do
  space
  exp <- f
  space
  return exp

pParens :: Parser p -> Parser p
pParens f = do
  void $ char '('
  exp <- f
  void $ char ')'
  return exp

pVar :: Parser LExpr
pVar = Var <$> lowerChar

pConst :: Parser LExpr
pConst = Const <$> parseIdentifier

pAbstraction :: Parser LExpr
pAbstraction = do
  void (try (string "|") <|> string lambdaSymbol) <?> "lambda"
  var <- pSpace (letterChar <?> "abstraction binding")
  void $ char '.'
  Abstraction var <$> parseLExpr

pApplication :: Parser LExpr
pApplication = do
  lhs <- try $ choice [pCurried, pAbstraction, pVar, pConst]
  space
  Application lhs
    <$> try (choice [pCurried, pAbstraction, pVar, pConst, pInner])

pCurried :: Parser LExpr
pCurried = do
  void (try (string "|") <|> string lambdaSymbol) <?> "lambda"
  bindings <- some (pSpace letterChar)
  void $ char '.'
  Curried bindings <$> parseLExpr

pInner :: Parser LExpr
pInner = pParens
  $ pSpace
  $ try
  $ choice [pApplication, pAbstraction, pCurried, pVar, pConst]

pLExpr :: Parser LExpr
pLExpr = pSpace $ try $ choice [pVar, pConst, pInner]

parseOuter :: Parser LExpr
parseOuter =
  pSpace $ try $ choice [pApplication, pCurried, pAbstraction, pVar, pConst]

parseLExpr :: Parser LExpr
parseLExpr = try parseOuter <|> pLExpr

pMain :: Parser LExpr
pMain = try parseLExpr <|> pParens pMain

parseAssignment :: Parser Expression
parseAssignment = do
  ident <- pSpace parseIdentifier
  char '='
  space
  Assignment ident <$> pMain

parseOut :: Parser Expression
parseOut = Out <$> pMain

parseExpression :: Parser Expression
parseExpression = try parseAssignment <|> parseOut

parseLCC :: Parser (Maybe Expression)
parseLCC = try (Just <$> parseExpression <* (try (void eol) <|> eof))
  <|> (eol >> return Nothing)
  <|> (eof >> return Nothing)
