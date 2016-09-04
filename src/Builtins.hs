{-# LANGUAGE ExistentialQuantification #-}
module Builtins (builtins, unpackBool, ioBuiltins, load) where

import Control.Monad.Except (throwError, catchError)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import System.IO

import Types
import Errors (liftThrows)
import qualified Parser

builtins :: [(String, [LispVal] -> ThrowsError LispVal)]
builtins = numbers ++ symbols ++ strings ++ binops ++ lists

expectArgs :: Integer -> [LispVal] -> ThrowsError LispVal
expectArgs args i = throwError $ NumArgs args i


--- coersions
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum lv@(String n) = let parsed = reads n in
                        if null parsed
                          then throwError $ TypeMismatch "Int" lv
                          else return $ fst $ parsed !! 0
unpackNum (List [n]) = return 0
unpackNum notNum = throwError $ TypeMismatch "number" notNum

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
isNumber ps = expectArgs 1 ps

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = expectArgs 2 []
numericBinop _ [a] = expectArgs 2 [a]
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op



strings = [
    ("string?", isString)
  ]

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString [_] = return $ Bool False
isString as = expectArgs 1 as

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal




symbols = [ ("symbol?", isSymbol)
          , ("string->symbol", string2symbol)
          , ("symbol->string", symbol2string)]

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol [_] = return $ Bool False
isSymbol as = expectArgs 1 as

symbol2string :: [LispVal] -> ThrowsError LispVal
symbol2string [Atom s] = return $ String s
symbol2string as = expectArgs 1 as

string2symbol :: [LispVal] -> ThrowsError LispVal
string2symbol [String s] = return $ Atom s
string2symbol as = expectArgs 1 as







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




lists = [
   ("car", car)
 , ("cdr", cdr)
 , ("cons", cons)
 , ("eq?", eqv)
 , ("eqv?", eqv)
 , ("equal?", equal)
 ]

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car as = expectArgs 1 as

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr as = expectArgs 1 as

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List xs] = return $ List $ [x1] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons as = expectArgs 2 as







eqv' :: LispVal -> LispVal -> Bool
eqv' (Bool arg1) (Bool arg2)             = arg1 == arg2
eqv' (Number arg1) (Number arg2)         = arg1 == arg2
eqv' (String arg1) (String arg2)         = arg1 == arg2
eqv' (Atom arg1) (Atom arg2)             = arg1 == arg2
eqv' (DottedList xs x) (DottedList ys y) = eqv' (List $ xs ++ [x]) (List $ ys ++ [y])
eqv' (List l) (List r)                   = (length l == length r) && (all (uncurry eqv') $ zip l r)
eqv' _ _                                 = False


eqv :: [LispVal] -> ThrowsError LispVal
eqv [l, r] = return $ Bool $ eqv' l r
eqv as = expectArgs 2 as


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
  unpacked1 <- unpacker arg1
  unpacked2 <- unpacker arg2
  return $ unpacked1 == unpacked2
  `catchError` (const $ return False)


equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  Bool eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ primitiveEquals || eqvEquals
equal l = expectArgs 2 l








ioBuiltins :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioBuiltins = [--("apply", applyProc),
              ("open-input-file", makePort ReadMode),
              ("open-output-file", makePort WriteMode),
              ("close-input-port", closePort),
              ("close-output-port", closePort),
              ("read", readProc),
              ("write", writeProc),
              ("read-contents", readContents),
              ("read-all", readAll)]
--
-- applyProc :: [LispVal] -> IOThrowsError LispVal
-- applyProc [func, List args] = apply func args
-- applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . Parser.readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . Parser.readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
