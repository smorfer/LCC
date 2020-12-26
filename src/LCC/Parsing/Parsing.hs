module LCC.Parsing.Parsing (parseLCC, parseExpression, Expression(..)) where

import           Text.Megaparsec
import           LCC.Language.Grammar (LExpr(Const, Application, Abstraction, Curried, Var))
import           Data.Void
import           Text.Megaparsec.Char
import           LCC.Utils.Text
import           Control.Monad
import           Data.List.NonEmpty
import           Data.Functor

type Parser = Parsec Void String

data Expression = Assignment String LExpr
                | Out LExpr
  deriving (Show)

bindingSymbols :: [String]
bindingSymbols = [lambdaSymbol, "|"]

parseIdentifier :: Parser String
parseIdentifier = some upperChar

-- Rule: Every parser should never handle spaces at the end of the line
pSpace :: Parser p -> Parser p
pSpace f = space *> f <* space

pParens :: Parser p -> Parser p
pParens f = char '(' *> pSpace f <* char ')'

pVar :: Parser LExpr
pVar = Var <$> lowerChar

pConst :: Parser LExpr
pConst = Const <$> parseIdentifier

pAbstraction :: Parser LExpr
pAbstraction = do
  void (try (string "|") <|> string lambdaSymbol) <?> "lambda"
  var <- pSpace letterChar <?> "abstraction binding"
  void $ char '.'
  space
  Abstraction var <$> choice [pLExpr, pParens pLExpr]

pCurried :: Parser LExpr
pCurried = do
  void (try (string "|") <|> string lambdaSymbol) <?> "lambda"
  bindings <- some (pSpace letterChar)
  void $ char '.'
  space
  Curried bindings <$> choice [pLExpr, pParens pLExpr]

pApplication :: Parser LExpr
pApplication = try
  $ do
    lhs <- choice [pParens pLExpr, pConst, pVar]
    space
    Application lhs <$> choice [pParens pLExpr, pConst, pVar]

pLExpr :: Parser LExpr
pLExpr = choice [pApplication, pVar, pConst, pCurried, pAbstraction]

parseStatement :: Parser LExpr
parseStatement = do
  stmnt <- choice [pLExpr, pParens pLExpr]
  space
  try (void eol) <|> eof
  return stmnt

parseAssign :: Parser String
parseAssign = do
  ident <- parseIdentifier
  space
  char '='
  return ident

parseAssignment :: Parser Expression
parseAssignment = do
  Assignment <$> (space >> parseAssign) <*> (space >> parseStatement)

parseOut :: Parser Expression
parseOut = Out <$> (space >> parseStatement)

parseExpression :: Parser Expression
parseExpression = try parseAssignment <|> parseOut

parseLCC :: Parser (Maybe Expression)
parseLCC = choice [Just <$> parseExpression, eol $> Nothing, eof $> Nothing]



