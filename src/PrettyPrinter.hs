module PrettyPrinter (showVal) where

import AST

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
