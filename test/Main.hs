{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Prelude hiding (any,takeWhile)
import Data.Char (ord,isDigit,chr)
import Data.ByteString.Parser
import Data.ByteString.Parser.Integral
import Control.Applicative
import Test.QuickCheck                      (Gen, Arbitrary(..), choose)
import Test.Framework                       (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           (Assertion,(@?=))
import Test.QuickCheck.Property             (failed,succeeded,Result(..))
import Data.Word
import qualified Data.ByteString.Char8 as BC8

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testCase "any" testAny
  , testCase "bad any" testBadAny
  , testCase "any and applicative" testAnyApplicative
  , testCase "takeWhile" testTakeWhile
  , testCase "many" testMany
  , testGroup "Word" $ integralTests word
  , testGroup "Word64" $ integralTests word64
  , testGroup "Word32" $ integralTests word32
  , testGroup "Word16" $ integralTests word16
  , testGroup "Word8" $ integralTests word8
  ]

integralTests :: forall i. (Integral i, Bounded i, Eq i, Num i, Show i) => Parser i -> [Test]
integralTests p =
  [ tryNum 1
  , tryNum 14
  , tryNum 200
  , tryNum (twoExp 8 - 1)
  , tryNum (twoExp 8)
  , tryNum (twoExp 8 + 1)
  , tryNum 298
  , tryNum (twoExp 16 - 1)
  , tryNum (twoExp 16)
  , tryNum (twoExp 16 + 1)
  , tryNum (twoExp 32 - 1)
  , tryNum (twoExp 32)
  , tryNum (twoExp 32 + 1)
  , tryNum (twoExp 64 - 1)
  , tryNum (twoExp 64)
  , tryNum (twoExp 64 + 1)
  , testCase "handles leftovers"
      $ parse p "11A" @?= ResultSuccess 11 "A"
  ]
  where
  twoExp :: Integer -> Integer
  twoExp a = 2 ^ a
  tryNum :: Integer -> Test
  tryNum x = testCase ("Number " ++ show x)
    $ parse p (BC8.pack (show x)) @?= 
    ( if x <= fromIntegral (maxBound :: i)
        then ResultSuccess (fromIntegral x :: i) ""
        else ResultFailure
    )
    

testTakeWhile :: Assertion
testTakeWhile = parse (takeWhile (/= c2w '-')) "11245-242" 
  @?= ResultSuccess "11245" "-242"

testBadAny :: Assertion
testBadAny = parse any "" 
  @?= ResultFailure

testMany :: Assertion
testMany = parse (many (satisfy (isDigit . w2c))) "42435" 
  @?= ResultSuccess (map c2w "42435") ""

testAny :: Assertion
testAny = parse any "1" 
  @?= ResultSuccess (c2w '1') ""

testAnyApplicative :: Assertion
testAnyApplicative = parse (any <* any) "12"
  @?= ResultSuccess (c2w '1') ""

c2w :: Char -> Word8
c2w = fromIntegral . ord

w2c :: Word8 -> Char
w2c = chr . fromIntegral
