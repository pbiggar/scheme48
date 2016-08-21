module Builtins (builtins) where

import AST

builtins :: [(String, ([LispVal] -> LispVal))]
builtins = numbers ++ symbols ++ strings


numbers = [
    ("number?", isNumber)
  , ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)]

isNumber :: [LispVal] -> LispVal
isNumber [(Number n)] = Bool True
isNumber _ = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0



strings = [("string?", isString)]

isString :: [LispVal] -> LispVal
isString [(String _)] = Bool True
isString _ = Bool False



symbols = [ ("symbol?", isSymbol)
          , ("string->symbol", string2symbol)
          , ("symbol->string", symbol2string)]

isSymbol :: [LispVal] -> LispVal
isSymbol [(Atom _)] = Bool True
isSymbol _ = Bool False

symbol2string :: [LispVal] -> LispVal
symbol2string [(Atom s)] = String s

string2symbol :: [LispVal] -> LispVal
string2symbol [(String s)] = Atom s
