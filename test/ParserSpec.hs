{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Telnet
import Telnet.Parser

import Data.Attoparsec.ByteString
import System.Console.ANSI
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "ANSI parser" $ do
    describe "parse" $ do
      it "can parse ANSI control codes" $ do
        parseOnly parseANSI "\ESC[0;32mgreen" `shouldBe` Right [Reset, SetColor Foreground Dull Green]
