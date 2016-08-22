module Parser (readExpr) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Control.Monad.Except (throwError)

import AST
import Errors

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space


parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many ((char '\\' >> (oneOf "\"\\rn")) <|> noneOf "\"")
  char '"'
  return $ String x


parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom


parseNumber :: Parser LispVal
parseNumber = many1 digit >>= (return . Number . read)

parseOct :: Parser LispVal
parseOct = do
  char '0'
  rest <- oneOf "1234567" >> many1 (oneOf "01234567")
  return $ (Number . fst . head . readOct) rest

parseHex :: Parser LispVal
parseHex = do
  char '0'
  oneOf "xX"
  rest <- many1 (oneOf "0123456789abcdefABCDEF")
  return $ (Number . fst . head . readHex) rest


parseCharacter :: Parser LispVal
parseCharacter = do
  char '#'
  char '\\'
  theChar <- noneOf ""
  return $ Character theChar

parseFloat :: Parser LispVal
parseFloat = do
  float <- many1 (oneOf "0123456789") >> char '.' >> many1 (oneOf "0123456789")
  return $ (Float . fst . head . readFloat) float


-- complex: 3+4i (unless i coef is 0, or )aa
-- real: #e1e10
-- rational 6/10

parseList :: Parser LispVal
parseList = do
  char '('
  list <- sepBy parseExpr spaces
  char ')'
  return $ List list

parseDottedList :: Parser LispVal
parseDottedList = do
  char '('
  h <- endBy parseExpr spaces
  char '.'
  spaces
  t <- parseExpr
  char ')'
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- #(0 (2 2 2 2) "Anna")
parseVector :: Parser LispVal
parseVector = do
  char '\''
  char '#'
  (List list) <- parseList
  return $ Vector list

parseExpr :: Parser LispVal
parseExpr = try parseCharacter
        <|> try parseString
        <|> try parseOct
        <|> try parseHex
        <|> try parseFloat
        <|> try parseNumber
        <|> try parseVector
        <|> try parseQuoted
        <|> try parseDottedList
        <|> try parseList
        <|> try parseAtom

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val
