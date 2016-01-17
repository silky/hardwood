{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Hardwood
import Hardwood.Parser

import Data.Attoparsec.ByteString
import Data.ByteString
import System.Console.ANSI
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "common parsing tasks" $ do
    describe "telnet" $ do
      it "can parse telnet commands" $ do
        parseOnly parseTelnetCommand (pack [255, 253, 201]) `shouldBe` (Right $ Do GMCP)
    describe "ANSI" $ do
     it "can parse ANSI control codes" $ do
       parseOnly parseANSI "\ESC[0;32mgreen" `shouldBe` Right [Reset, SetColor Foreground Dull Green]
