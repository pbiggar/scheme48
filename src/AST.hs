module AST where

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
