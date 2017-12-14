{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (takeWhile)
import Data.Char
import qualified Data.Text as T
import Data.Attoparsec.Text hiding (take)
import Control.Applicative 
-- import Skirl
-- import Lib

main :: IO ()
main = do
  print $ parse decimal "1000" `feed` ""
  print $ parse node "  abc_2 " `feed` ""

-- |
--

newtype NODE = NODE String deriving (Show)
type INPUT = NODE
type OUTPUT = NODE
type INPUTS = [INPUT]
type OUTPUTS = [OUTPUT]
data SENS = Posedge NODE | Negedge NODE | Edge NODE deriving (Show)
type SENSLST = [SENS]
type EXPR = String

data COMB = COMB EXPR deriving (Show)
data SEQL = SEQL SENSLST [COMB] deriving (Show)
data MOD = MOD INPUTS OUTPUTS [COMB] [SEQL] deriving (Show)
type INSTNAME = String
type MODNAME = String
data INST = INST MODNAME INSTNAME [NODE]


-- |
--
-- >>> parse (token digit) "  123  4"
-- "Done \"4\" \"123\""
token :: Parser a -> Parser [a]
token p = skipSpace *> (many p) <* skipSpace

isNodeChar :: Char -> Bool
isNodeChar x = or $ fmap ($ x) [isAlphaNum, (== '_')]

isEndOfExpr :: Char -> Bool
isEndOfExpr = (== ';')

isSpecialChar :: Char -> Bool
isSpecialChar c = any (== c) ['(', ')', '\n', '\t', ' ', ',', ';']

node :: Parser NODE
node = NODE <$> token (satisfy isNodeChar)

sens :: Parser SENS
sens = do s <- token (satisfy isAlpha) 
          n <- node
          return $ case s of
                     "posedge" -> Posedge n
                     "negedge" -> Negedge n
                     otherwise -> Edge n 

expr :: Parser EXPR
expr = many $ satisfy (not . isEndOfExpr)

comb :: Parser COMB
comb = do string "assign"
          skipSpace
          o <- node
          string "="
          skipSpace
          e <- expr
          return $ COMB e

seql :: Parser SEQL
seql = do token $ string "always"
          token $ char '@'
          token $ char '('
          ss <- sepBy sens (char ',')
          token $ char ')'
          es <- expr  -- begin end style is not parsed
          return $ SEQL ss [COMB es]
          

mod :: Parser MOD
mod = undefined

modname :: Parser MODNAME
modname = token (satisfy isAlphaNum)

instname :: Parser INSTNAME
instname = token (satisfy isAlphaNum)

inst :: Parser INST
inst = do m <- modname
          i <- instname
          token $ char '('
          ns <- sepBy node (char ',')
          token $ char ')'
          token $ char ';'
          return $ INST m i ns

xxx = "module counter (clk, rst, dout); \
      \  reg [3:0] cnt; \
      \  always @(posedge clk) begin \
      \    if (rst) \
      \      cnt <= 4'h0; \
      \    else \
      \      cnt <= cnt + 1; \
      \  end\
      \ "
