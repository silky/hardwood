{- LANGUAGE OverloadedStrings -}

module Telnet.Parser
    ( parseANSI
    , Command (..)
    , Option (..)
    , TelnetCommand (..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.ByteString
import Data.Char
import System.Console.ANSI

data Command = Do
             | Dont
             | Will
             | Wont
             | SE
             | SB
             deriving (Show, Eq, Read)

instance Enum Command where
  fromEnum Do = 253
  fromEnum Dont = 254
  fromEnum Will = 251
  fromEnum Wont = 252
  fromEnum SE = 240
  fromEnum SB = 250
  toEnum 253 = Do
  toEnum 254 = Dont
  toEnum 251 = Will
  toEnum 252 = Wont
  toEnum 240 = SE
  toEnum 250 = SB

data Option = GMCP deriving (Show, Eq, Read)

instance Enum Option where
  fromEnum GMCP = 201
  toEnum 201 = GMCP

data TelnetCommand = TelnetCommand Command (Maybe Option) deriving (Show, Eq, Read)

parseTelnetCommand :: Parser TelnetCommand
parseTelnetCommand = do
  iac
  cmd <- parseCommand
  opt <- option Nothing parseOption
  return $ TelnetCommand cmd opt
    where iac = word8 255
          parseCommand = undefined
          parseOption = undefined

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
         where reset = liftM digitToInt $ char '0'
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
