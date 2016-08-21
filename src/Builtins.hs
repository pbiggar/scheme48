module Builtins (builtins) where

import AST

builtins :: [(String, ([LispVal] -> LispVal))]
builtins = [
    ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("string?", isString)
  , ("number?", isNumber)
  , ("symbol?", isSymbol)]

isSymbol :: [LispVal] -> LispVal
isSymbol [(Atom _)] = Bool True
isSymbol _ = Bool False

isString :: [LispVal] -> LispVal
isString [(String _)] = Bool True
isString _ = Bool False

isNumber :: [LispVal] -> LispVal
isNumber [(Number n)] = Bool True
isNumber _ = Bool False


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
-- unpackNum (String s) = let parsed = reads s :: [(Integer, String)] in
--                           if null parsed
--                             then 0
--                             else fst . head $ parsed
unpackNum _ = 0
