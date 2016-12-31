{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

module Data.ByteString.Parser.Int where

import Data.ByteString (ByteString)
import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.ForeignPtr
import GHC.Ptr (Ptr(..))
import GHC.Int (Int(..))
import GHC.Word (Word(..))
import Data.Bits
import Foreign.Storable
import GHC.Prim
import qualified Data.ByteString.Internal as B

parseWord64 :: ByteString -> IO (Word64,Int)
parseWord64 (B.PS fptr off len) = if off >= 8 && len >= 32
  then withForeignPtr fptr $ \ptr -> do
    let (aptr :: Ptr Word64,!backBy) = alignPtrAlt ptr 8
        !backByBits = unsafeShiftL backBy 3
        !forwardByBits = 64 - backByBits
    w1 <- peek aptr 
    w2 <- peek (plusPtr aptr 8)
    w3 <- peek (plusPtr aptr 16)
    w4 <- peek (plusPtr aptr 24)
    let !w1' = unsafeShiftL w1 backByBits .|. unsafeShiftR w2 forwardByBits
        !w2' = unsafeShiftL w2 backByBits .|. unsafeShiftR w3 forwardByBits
        !w3' = unsafeShiftL w3 backByBits .|. unsafeShiftR w4 forwardByBits
        !numberLen = findNumberLength w1' w2' w3'
        !result = fromAsciiBytes numberLen w1' w2' w3'
        !len1 = unsafeShiftL 65536 (fromIntegral (8 - firstByteBetween w1' 47 58))
        !len2 = unsafeShiftL 256 (fromIntegral (8 - firstByteBetween w2' 47 58))
        !len3 = unsafeShiftL 1 (fromIntegral (8 - firstByteBetween w3' 47 58 + 16))
        !finalLen = countLeadingZeros (len1 .|. len2 .|. len3)
    return (result,numberLen)
  else byteByByte (B.PS fptr off len)

findNumberLength :: Word64 -> Word64 -> Word64 -> Int
findNumberLength = error "write me"

byteByByte :: ByteString -> IO (Word64,Int)
byteByByte = error "write me"

alignPtrAlt :: Ptr a -> Int -> (Ptr b,Int)
alignPtrAlt addr@(Ptr a) (I# i)
  = case remAddr# a i of {
      0# -> (castPtr addr,0);
      r -> (Ptr (plusAddr# a (0# -# r)), I# r) }
  
-- | Consumes 24 bytes. There are a maximum of
--   19 decimal characters needed to represent
--   2^64.
{-# INLINE fromAsciiBytes #-}
fromAsciiBytes :: Int -> Word64 -> Word64 -> Word64 -> Word64
fromAsciiBytes !len !a !b !c =
    getDecimal 0 + getDecimal 1 + getDecimal 2 + getDecimal 3 + getDecimal 4 
  + getDecimal 5 + getDecimal 6 + getDecimal 7 + getDecimal 8 + getDecimal 9 
  + getDecimal 10 + getDecimal 11 + getDecimal 12 + getDecimal 13 + getDecimal 14 
  + getDecimal 15 + getDecimal 16 + getDecimal 17 + getDecimal 18 + getDecimal 19 
  where 
  {-# INLINE getByte #-}
  getByte :: Int -> Word64
  getByte !i = 255 .&. case i of
    0 -> unsafeShiftR a 56
    1 -> unsafeShiftR a 48
    2 -> unsafeShiftR a 40
    3 -> unsafeShiftR a 32
    4 -> unsafeShiftR a 24
    5 -> unsafeShiftR a 16
    6 -> unsafeShiftR a 8
    7 -> unsafeShiftR a 0
    _ -> 0
  {-# INLINE getDecimal #-}
  getDecimal :: Int -> Word64
  getDecimal !i = if i < len
    then getByte i * tenExp (len - i - 1)
    else 0

tenExp :: Int -> Word64
tenExp !x = case x of
  0 -> 1
  1 -> 10
  2 -> 100
  3 -> 1000
  4 -> 10000
  5 -> 100000
  6 -> 1000000
  7 -> 10000000
  8 -> 100000000
  9 -> 1000000000
  10 -> 10000000000
  11 -> 100000000000
  12 -> 1000000000000
  13 -> 10000000000000
  14 -> 100000000000000
  15 -> 1000000000000000
  16 -> 10000000000000000
  17 -> 100000000000000000
  18 -> 1000000000000000000
  19 -> 10000000000000000000
  _ -> 0
{-# INLINE tenExp #-}


hasBetween# :: Word# -> Word# -> Word# -> Word#
hasBetween# x m n =
  and#
  ( minusWord#
    ( timesWord# alternating (plusWord# 127## n) )
    ( and# x ( timesWord# alternating 127## ) )
  )
  ( and# (not# x)
    ( and#
      ( plusWord#
        ( and# x (timesWord# alternating 127## ) )
        ( timesWord# alternating (minusWord# 127## m) )
      )
      ( timesWord# alternating 128## )
    )
  )
  where
  alternating :: Word#
  alternating = quotWord# (not# 0##) 255##

hasLess# :: Word# -> Word# -> Word#
hasLess# x n =
  and#
    (minusWord# x (timesWord# alternating n))
    (and#
      (not# x)
      (timesWord# alternating 128##)
    )
  where
  alternating :: Word#
  alternating = quotWord# (not# 0##) 255##

-- ((~0UL/255*(127+(n))-((x)&~0UL/255*127)&~(x)&((x)&~0UL/255*127)+~0UL/255*(127-(m)))&~0UL/255*128)
-- hasless(x,n) (((x)-~0UL/255*(n))&~(x)&~0UL/255*128)

hasBetween :: 
     Word -- ^ target
  -> Word -- ^ lower
  -> Word -- ^ upper
  -> Word
hasBetween (W# a) (W# b) (W# c) = W# (hasBetween# a b c)

firstByteBetween :: Word -> Word -> Word -> Word
firstByteBetween a b c = unsafeShiftR (fromIntegral (countLeadingZeros (hasBetween a b c))) 3

hasLess :: Word -> Word -> Word
hasLess (W# a) (W# b) = W# (hasLess# a b)
