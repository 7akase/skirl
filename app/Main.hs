{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.Attoparsec.Text hiding (take)
import Control.Applicative 
-- import Skirl
-- import Lib

main :: IO ()
main = do
  print $ parse decimal "1000" `feed` ""
  print $ parse ymdParser "19800929" `feed` ""
  let sample_text = "19800929"
  print $ parse ymdParser sample_text

twoOfDecimal :: Parser (Int, Int)
twoOfDecimal = do
  left <- decimal
  char ','
  right <- decimal
  return (left, right)


-- data LOGIC = FF <|> CL

data YMD = YMD Int Int Int deriving Show

countRead :: Read a => Int -> Parser Char -> Parser a
countRead i = fmap read . count i

ymdParser :: Parser YMD
ymdParser = YMD <$> countRead 4 digit <*> countRead 2 digit <*> countRead 2 digit


data INST = INST String String deriving Show


-- instParser :: Parser INST
-- -- parse instParser "block block_u (a, b, c);" --> INST "block" "block_u"
-- instParser = INST <$> takeToken <*> takeToken

takeToken :: Parser T.Text
takeToken = Data.Attoparsec.Text.takeWhile (not . isHorizontalSpace)

