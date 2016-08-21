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

--instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (Atom s) = s
showVal (List ([Atom "quote", rest])) = "'" ++ showVal rest
showVal (List as) = "(" ++ unwordsList as ++ ")"
showVal (String s) = show s
showVal (Number n) = show n
showVal (Character c) = "\'" ++ [c]
showVal (Vector as) = "'#(" ++ (unwordsList as) ++ ")"
showVal (DottedList head tail) = "(" ++ (unwordsList head) ++ " . " ++ (showVal tail) ++ ")"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
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

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval val@(Bool _) = val
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Character _) = val
eval (List (Atom func : args)) = apply func $ map eval args
--eval (Vector as) = "'#(" ++ (unwordsList as) ++ ")"
--eval (DottedList head tail) = "(" ++ (unwordsList head) ++ " . " ++ (eval tail) ++ ")"

eval other = String $ "Not supported yet: " ++ show other

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) (lookup func primitives)

primitives :: [(String, ([LispVal] -> LispVal))]
primitives = [
  ("+", numericBinop (+))
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String s) = let parsed = reads s :: [(Integer, String)] in
                          if null parsed
                            then 0
                            else fst . head $ parsed
unpackNum _ = 0






main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ "Args are:  " ++ expr

  let parsed = readExpr expr
  putStrLn $ "Parsed:    " ++ (showVal parsed)

  let evaled = eval parsed
  putStrLn $ "Evaled:    " ++ (showVal evaled)

  putStrLn $ "Structure: " ++ (show parsed)
  Exit.exitFailure
  --if (showVal parsed) == expr then Exit.exitSuccess else Exit.exitFailure
