{-# LANGUAGE OverloadedStrings #-}

module Hardwood.Parser
    ( parseANSI
    , parseTelnetCommand
    , parseGMCP
    , TelnetCommand (..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char, space, decimal)
import qualified Data.Attoparsec.ByteString.Char8 as C
import Data.ByteString
import Data.Char
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Word
import System.Console.ANSI

data TelnetCommand = Do Word8
                   | Dont Word8
                   | Will Word8
                   | Wont Word8
                   deriving (Show, Eq, Read)

parseTelnetCommand :: Parser TelnetCommand
parseTelnetCommand = do
  iac
  cmd <- parseCommand
  opt <- anyWord8
  return $ cmd opt
    where iac = word8 255
          parseCommand = (word8 253 >> return Do)
                     <|> (word8 254 >> return Dont)
                     <|> (word8 251 >> return Will)
                     <|> (word8 252 >> return Wont)

escapedIAC :: Parser Word8
escapedIAC = word8 255 >> word8 255 >> return 255

parseGMCP :: Parser (Map.Map T.Text T.Text)
parseGMCP = do
  iac
  sb
  gmcp
  key <- T.pack <$> moduleName
  space
  val <- decodeUtf8 <$> jsonContent
  word8 se
  return $ Map.singleton key val
  where iac = word8 255
        sb = word8 250
        se = 240
        gmcp = word8 201
        moduleName = many1 . C.satisfy $ C.inClass "a-zA-Z."
        jsonContent = takeTill (==se)

-- Rudimentary ANSI control code parser.
-- Supported features:
-- * SGR (Colors & Reset only)
parseANSI :: Parser [SGR]
parseANSI = do
  csi
  args <- validArg `sepBy` char ';'
  code <- char 'm'
  return $ decodeANSI (code, Prelude.reverse args) []
  where csi = char '\ESC' >> char '['

decodeANSI :: (Char, [Int]) -> [SGR] -> [SGR]
decodeANSI (code@'m', 0:xs) sgrs =
  decodeANSI (code, xs) (Reset : sgrs)
decodeANSI (code@'m', x:xs) sgrs =
  decodeANSI (code, xs) (lookupColor x : sgrs)
decodeANSI (_, []) sgrs = sgrs
decodeANSI _ _ = error "parsed an invalid or unimplemented ANSI control code."

validArg :: Parser Int
validArg = reset
       <|> fgDull
       <|> fgVivid
       <|> bgDull
       <|> bgVivid
         where reset = digitToInt <$> char '0'
               fgDull = decimalRange 30 37
               fgVivid = decimalRange 90 97
               bgDull = decimalRange 40 47
               bgVivid = decimalRange 100 107

lookupColor :: Int -> SGR
lookupColor arg
  | arg >= 30 && arg <= 37 =
    SetColor Foreground Dull (toEnum $ arg - 30)
  | arg >= 40 && arg <= 47 =
    SetColor Background Dull (toEnum $ arg - 40)
  | arg >= 90 && arg <= 97 =
    SetColor Foreground Vivid (toEnum $ arg - 90)
  | arg >= 100 && arg <= 107 =
    SetColor Background Vivid (toEnum $ arg - 100)
  | otherwise =
    error "attempted to decode something that shouldn't have been parsed."

decimalRange :: Int -> Int -> Parser Int
decimalRange lower upper = do
  d <- decimal
  if d >= lower && d <= upper then return d else fail "decimalRange"


