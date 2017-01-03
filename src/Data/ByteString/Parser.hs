{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -Wall #-}

module Data.ByteString.Parser 
  ( Parser(..)
  , Result(..)
  , replicate
  , replicateUnbox
  , satisfy
  , any
  , takeWhile
  , takeTillWord8
  , endOfInput
  , parse
  -- Internal stuff
  , Step(..)
  , success
  , failure
  ) where

import Prelude hiding (replicate,any,takeWhile)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Unsafe.Coerce (unsafeCoerce)
import GHC.Prim (Any)
import Control.Applicative
import Control.Monad.ST
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as MVG

data Result a
  = ResultSuccess !a {-# UNPACK #-} !ByteString
  | ResultFailure
  deriving (Functor,Eq,Show)

data Step a = Step
  { stepValue :: !Any
    -- ^ The result
  , stepSuccess :: {-# UNPACK #-} !Int
    -- ^ Whether or not it has succeeded, should be 1 or 0
  , stepMovement :: {-# UNPACK #-} !Int
    -- ^ How far forward to move in the 'ByteString'.
  } deriving (Functor)

data Safe a
  = SafeSuccess !a {-# UNPACK #-} !Int
  | SafeFailure

newtype Parser a = Parser (ByteString -> Step a)

data Placeholder = Placeholder

viewStep :: Step a -> Safe a
viewStep (Step val status movement) = if status == 1
  then SafeSuccess (fromAny val) movement
  else SafeFailure
{-# INLINE viewStep #-}

instance Functor Parser where
  fmap g (Parser f) = Parser $ \bs -> case viewStep (f bs) of
    SafeFailure -> failure
    SafeSuccess a move -> success (g a) move

instance Applicative Parser where
  pure a = Parser (const (success a 0))
  Parser f <*> Parser g = Parser $ \bs -> case viewStep (f bs) of
    SafeFailure -> failure
    SafeSuccess a moveA -> case viewStep (g (BSU.unsafeDrop moveA bs)) of
      SafeFailure -> failure
      SafeSuccess b moveB -> success (a b) (moveA + moveB)

instance Alternative Parser where
  empty = never
  (<|>) = alt
  many = manyX

toAny :: a -> Any
toAny = unsafeCoerce

fromAny :: Any -> a
fromAny = unsafeCoerce

success :: a -> Int -> Step a
success val movement = Step (toAny val) 1 movement
{-# INLINE success #-}

failure :: Step a
failure = Step (toAny Placeholder) 0 0
{-# INLINE failure #-}

parse :: Parser a -> ByteString -> Result a
parse (Parser f) bs = case viewStep (f bs) of
  SafeSuccess val move -> ResultSuccess val (BSU.unsafeDrop move bs)
  SafeFailure -> ResultFailure

-- | This parser will succeed no matter what.
takeWhile :: (Word8 -> Bool) -> Parser ByteString
takeWhile p = Parser $ \bs -> 
  let bs1 = BS.takeWhile p bs
   in success bs1 (BS.length bs1)

takeTillWord8 :: Word8 -> Parser ByteString
takeTillWord8 w = Parser $ \bs -> case BS.elemIndex w bs of
  Nothing -> failure
  Just i -> success (BSU.unsafeTake i bs) i

endOfInput :: Parser ()
endOfInput = Parser $ \bs -> if BS.null bs
  then success () 0 
  else failure

any :: Parser Word8
any = Parser $ \bs -> if BS.length bs > 0
  then success (BSU.unsafeHead bs) 1
  else failure

satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy f = Parser $ \bs -> if BS.length bs > 0
  then let w = BSU.unsafeHead bs in if f w
    then success w 1
    else failure
  else failure

manyX :: Parser a -> Parser [a]
manyX p = many_p
  where many_p = some_p <|> pure []
        some_p = (:) <$> p <*> many_p

replicate :: Int -> Parser a -> Parser (V.Vector a)
replicate = replicateGeneric

replicateUnbox :: MVU.Unbox a => Int -> Parser a -> Parser (VU.Vector a)
replicateUnbox = replicateGeneric

replicateGeneric :: VG.Vector v a => Int -> Parser a -> Parser (v a)
replicateGeneric !total (Parser f) = Parser $ \ !bs -> runST $ do
  mv <- MVG.new total
  go1 bs mv
  where
  go1 !bs !mv = go2 0 0
    where 
    go2 !n !move1
      | n < total = case viewStep (f (BSU.unsafeDrop move1 bs)) of
          SafeFailure -> return failure
          SafeSuccess !val !move2 -> do
            MVG.write mv n val
            go2 (n + 1) move2
      | otherwise = do
          v <- VG.unsafeFreeze mv
          return (success v move1)

never :: Parser a
never = Parser $ \_ -> failure

alt :: Parser a -> Parser a -> Parser a
alt (Parser f) (Parser g) = Parser $ \bs -> case viewStep (f bs) of
  SafeSuccess val move -> success val move
  SafeFailure -> case viewStep (g bs) of
    SafeSuccess val move -> success val move
    SafeFailure -> failure

-- decimalMaxDigits :: Int -> Parser Int
-- decimalMaxDigits !maxDigits = Parser go1
--   where
--   go1 !bs = go2
--     where 
--     go2 =
--       if check 0
--         then
--     doneAt !n = maxDigits <= n || BS.length bs <= n
--   if maxDigits > 0 && len > 0
--   if BS.length bs > maxDigits + 1
--     then 
--   
-- decimalHelper :: Int -> ByteString -> Maybe Int
-- decimalHelper maxToTake bs =
--   let a = BSU.
 


