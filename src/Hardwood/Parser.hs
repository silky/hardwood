{-# LANGUAGE OverloadedStrings #-}

module Hardwood.Parser
    ( telnetMessage
    , TelnetCommand (..)
    , TelnetMessage (..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char)
import qualified Data.Attoparsec.ByteString.Char8 as C
import Data.ByteString
import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Word
import System.Console.ANSI

-- TODO: change GMCP to a mapping from T.Text to JSON Objects (once aeson parsing is implemented)
-- TODO: change [SGR] to a different way of storing color options besides SGR because
--       System.Console.ANSI isn't going to be used for display.
data TelnetMessage = Cmd TelnetCommand
                   | GMCP (Map.Map T.Text T.Text)
                   | ANSI [SGR]
                   | GameText Char
                   deriving (Show, Eq)

parseAll :: Parser [TelnetMessage]
parseAll = many1 telnetMessage

telnetMessage :: Parser TelnetMessage
telnetMessage = parseTelnetCmd
                 <|> parseGMCP
                 <|> parseANSI
                 <|> parseGameText

data TelnetCommand = Do Word8
                   | Dont Word8
                   | Will Word8
                   | Wont Word8
                   deriving (Show, Eq, Read)

parseTelnetCmd :: Parser TelnetMessage
parseTelnetCmd = do
  iac
  cmd <- parseCommand
  opt <- anyWord8
  return . Cmd $ cmd opt
    where iac = word8 255
          parseCommand = (word8 253 >> return Do)
                     <|> (word8 254 >> return Dont)
                     <|> (word8 251 >> return Will)
                     <|> (word8 252 >> return Wont)

escapedIAC :: Parser Word8
escapedIAC = word8 255 >> word8 255 >> return 255

-- TODO: convert the value to a JSON object using aeson
parseGMCP :: Parser TelnetMessage
parseGMCP = do
  iac
  sb
  gmcp
  key <- T.pack <$> moduleName
  C.space
  val <- decodeUtf8 <$> jsonContent
  word8 se
  return . GMCP $ Map.singleton key val
  where iac = word8 255
        sb = word8 250
        se = 240
        gmcp = word8 201
        moduleName = many1 . C.satisfy $ C.inClass "a-zA-Z."
        jsonContent = takeTill (==se)

-- Rudimentary ANSI control code parser.
-- Supported features:
-- * SGR (Colors & Reset only)
parseANSI :: Parser TelnetMessage
parseANSI = do
  csi
  args <- validArg `sepBy` char ';'
  code <- char 'm'
  return . ANSI $ decodeANSI (code, Prelude.reverse args) []
  where csi = char '\ESC' >> char '['

-- TODO: Decode to another color format besides SGR, System.Console.ANSI is
--       not going to be used for display.
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
  d <- C.decimal
  if d >= lower && d <= upper then return d else fail "decimalRange"

parseGameText :: Parser TelnetMessage
parseGameText = do
  c <- C.anyChar
  return . GameText $ c


