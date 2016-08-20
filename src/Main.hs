module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.List (intersperse)
import System.Exit as Exit

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Float Float
             | Bool Bool
             | Vector [LispVal]
             deriving (Show)



showVal :: LispVal -> String
showVal (Atom s) = s
showVal (List ((Atom "quote"):rest:[])) = "'" ++ showVal rest
showVal (List as) = "(" ++ (concat (intersperse " " (map showVal as))) ++ ")"
showVal (String s) = "\"" ++ s ++ "\"" -- TODO: escape strings
showVal (Number n) = show n
showVal (Character c) = "\'" ++ [c]
showVal (Vector as) = "'#(" ++ (concat (intersperse " " (map showVal as))) ++ ")"
showVal (DottedList head tail) = "(" ++ (concat (intersperse " " (map showVal head))) ++ " . " ++ (showVal tail) ++ ")"
showVal other = "Not supported yet: " ++ show other

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




readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ showVal val ++ "\n" ++ "(structurally: " ++ show val ++ ")"


main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ "Args are:    " ++ expr
  putStrLn $ readExpr expr
  Exit.exitFailure
