{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Data.ByteString.Parser.Integral 
  ( word
  , word64
  , word32
  , word16
  , word8
  ) where

import Data.ByteString.Parser
import Data.ByteString (ByteString)
import Data.Word
import Data.Coerce (coerce)
import Foreign.Storable (sizeOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

word :: Parser Word
word = case sizeOf (undefined :: Word) of
  8 -> coerce word64
  4 -> coerce word32
  _ -> error "word parser only works on 32-bit or 64-bit architecture"

word64 :: Parser Word64
word64 = wordGen 1844674407370955161 6

word32 :: Parser Word32
word32 = wordGen 429496729 6

word16 :: Parser Word16
word16 = coerce (wordGen 6553 6 :: Parser Word)

word8 :: Parser Word8
word8 = coerce (wordGen 25 6 :: Parser Word)

wordGen :: forall i. Integral i => i -> i -> Parser i
wordGen !upperBoundDivTen !lastDigit = Parser $ \ !bs -> if BS.length bs > 0
  then case decimalDigitToWord (BSU.unsafeHead bs) of
    Nothing -> failure
    Just w -> go1 bs w 1
  else failure
  where
  go1 :: ByteString -> i -> Int -> Step i
  go1 !bs = go2
    where
    go2 :: i -> Int -> Step i
    go2 !wprev !ix = if ix < BS.length bs
      then case decimalDigitToWord (BSU.unsafeIndex bs ix) of
        Nothing -> success wprev ix
        Just !w -> if wprev < upperBoundDivTen
          then go2 (wprev * 10 + w) (ix + 1)
          else if w < lastDigit
            then let !ixNext = ix + 1 in
              if ixNext < BS.length bs
                then case decimalDigitToWord (BSU.unsafeIndex bs ixNext) of
                  Nothing -> success (wprev * 10 + w) ix
                  Just _ -> failure
                else success (wprev * 10 + w) ixNext
            else failure
      else success wprev ix
  decimalDigitToWord :: Word8 -> Maybe i
  decimalDigitToWord w8 =
    let !d = fromIntegral w8 - 48
     in if d > 9 then Nothing else Just d

-- 2 ^ 64 = 18446744073709551616
-- 2 ^ 32 = 4294967296
-- 2 ^ 16 = 65536
-- 2 ^ 8 = 256

