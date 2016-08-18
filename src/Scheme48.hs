module Scheme48 (isLeapYear) where

isLeapYear y
  | y `mod` 400 == 0 = True
  | y `mod` 100 == 0 = False
  | y `mod` 4 == 0 = True
  | otherwise = False
