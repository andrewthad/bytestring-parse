{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.ByteString.Incremental where

import qualified Data.ByteString.Streaming as SB

data Result r m a
  = ResultSuccess !a !(SB.ByteString m r)
  | ResultFailure
  deriving (Functor)

newtype Parser m a = Parser (forall r. SB.ByteString m r -> m (Result r m a))

deriving instance Functor m => Functor (Parser m)

instance Monad m => Applicative (Parser m) where
  pure a = Parser $ \s -> return $ ResultSuccess a s
  Parser f <*> Parser g = Parser $ \s1 -> do
    resFunc <- f s1
    case resFunc of
      ResultFailure -> return ResultFailure
      ResultSuccess func s2 -> do
        resVal <- g s2
        case resVal of
          ResultFailure -> return ResultFailure
          ResultSuccess val s3 -> do
             return (ResultSuccess (func val) s3)




