module Builtins (builtins, unpackBool) where

import Control.Monad.Except (throwError)

import AST
import Errors


builtins :: [(String, [LispVal] -> ThrowsError LispVal)]
builtins = numbers ++ symbols ++ strings ++ binops



--- coersions
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum lv@(String n) = let parsed = reads n in
                        if null parsed
                          then throwError $ TypeMismatch "Int" lv
                          else return $ fst $ parsed !! 0
unpackNum (List [n]) = return 0

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Number b) = return $ if b == 0 then False else True
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool





numbers = [
    ("number?", isNumber)
  , ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  ]

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number n] = return $ Bool True
isNumber [_] = return $ Bool False
isNumber ps = throwError $ NumArgs 1 ps

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs  2 []
numericBinop _ [a] = throwError $ NumArgs  2 [a]
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op



strings = [
    ("string?", isString)
  ]

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString [_] = return $ Bool False
isString as = throwError $ NumArgs 1 as

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal




symbols = [ ("symbol?", isSymbol)
          , ("string->symbol", string2symbol)
          , ("symbol->string", symbol2string)]

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol [_] = return $ Bool False
isSymbol as = throwError $ NumArgs 1 as

symbol2string :: [LispVal] -> ThrowsError LispVal
symbol2string [Atom s] = return $ String s
symbol2string as = throwError $ NumArgs 1 as

string2symbol :: [LispVal] -> ThrowsError LispVal
string2symbol [String s] = return $ Atom s
string2symbol as = throwError $ NumArgs 1 as







binops = [
  ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  ]

strBoolBinop = boolBinop unpackStr
numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
   if length args /= 2
     then throwError $ NumArgs 2 args
     else do
       left <- unpacker $ args !! 0
       right <- unpacker $ args !! 1
       return $ Bool $ left `op` right
