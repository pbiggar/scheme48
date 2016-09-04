{-# LANGUAGE FlexibleInstances #-}
module Types where

import qualified Text.ParserCombinators.Parsec as Parsec
import Data.IORef (IORef)


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Float Float
             | Bool Bool
             | Vector [LispVal]
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String],
                      vararg :: (Maybe String),
                      body :: [LispVal],
                      closure :: Env } deriving (Show)

instance Show ([LispVal] -> ThrowsError LispVal) where show = \_ -> "<throws>"
instance Show Env where show = \_ -> "<env>"


type Env = IORef [(String, IORef LispVal)]

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser Parsec.ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

--instance Show LispVal where show = showVal
