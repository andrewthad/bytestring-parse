{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Data.ByteString.Another where

data BoundedResult a
  = BoundedResultSuccess !a !Int
  | BoundedResultFailure
  deriving (Functor)

newtype Parser a 
  = ParserNormal (ByteString -> BoundedResult a)
  | ParserBounded !Int (ByteString -> BoundedResult a) 
    -- ^ Amount of input consumed has an upper bound

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





