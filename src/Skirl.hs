module Skirl where

import Data.Attoparsec.Text hiding (take)

twoOfDecimal :: Parser (Int, Int)
twoOfDecimal = do
  left <- decimal
  char ','
  right <- decimal
  return (left, right)

  
