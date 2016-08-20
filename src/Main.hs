module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Float Float
             | Bool Bool deriving (Show)


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
  prefix <- many (oneOf "0123456789")
  char '.'
  suffix <- many (oneOf "0123456789")
  return $ (Float . fst . head . readFloat) (prefix ++ "." ++ suffix)


parseExpr :: Parser LispVal
parseExpr = try parseCharacter
        <|> parseAtom
        <|> parseString
        <|> try parseOct
        <|> try parseHex
        <|> try parseFloat
        <|> parseNumber



readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val


main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
