{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Aeson.Free where

import Prelude hiding ((.),id)
import Data.Text (Text)
import Data.Foldable (toList)
import Control.Monad
import Control.Category as C
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap

data Free cat a b where
  Pure :: b -> Free cat a b
  Cat :: cat a b -> Free cat a b
  Ap :: Free cat c a -> Free cat c (a -> b) -> Free cat c b
  Compose :: Free cat b c -> Free cat a b -> Free cat a c
  Id :: Free cat a a

class Functor1 f where
  fmap1 :: (a -> b) -> f c a -> f c b

instance Functor (Free cat a) where
  fmap f x = case x of
    Pure b -> Pure (f b)
    Compose m n -> Compose (fmap f m) n
    Ap a y -> Ap a (fmap (f .) y)
    Cat c -> Ap (Cat c) (Pure f) -- Cat (fmap1 f c)
    Id -> Ap Id (Pure f)

instance C.Category (Free cat) where
  id = Id
  (.) = Compose

instance Applicative (Free cat a) where
  pure b = Pure b
  x <*> y = Ap y x

newtype MyError a = MyError { getMyError :: Either String [a] }
  deriving (Functor)

instance Applicative MyError where
  pure a = MyError (Right [a])
  MyError (Left e) <*> MyError (Right _) = MyError $ Left e
  MyError (Right _) <*> MyError (Left e) = MyError $ Left e
  MyError (Right xs) <*> MyError (Right ys) = MyError $ Right (xs <*> ys)

instance Monad MyError where
  return = pure
  MyError (Left e) >>= _ = MyError $ Left e
  MyError (Right xs) >>= f = MyError $ fmap join $ mapM (getMyError . f) xs

newtype JsonParse a b = JsonParse { getJsonParse :: a -> MyError b }
  deriving (Functor)

data J2 a b where
  J2A :: (a -> MyError b) -> J2 a b
  J2B :: J2 a [a]

instance Functor1 JsonParse where
  fmap1 = fmap

run :: Monad g => (forall x y. cat y x -> y -> g x) -> Free cat a b -> a -> g b
run f x a = case x of
  Id -> pure a
  Compose m n -> (run f m <=< run f n) a
  Cat c -> f c a
  Pure b -> pure b
  Ap m n -> do
    func <- run f n a
    val <- run f m a
    return (func val)

runX :: Monad m => (forall x y. cat y x -> m y -> m x) -> Free cat a b -> m a -> m b
runX f x a = case x of
  Id -> a
  Compose m n -> runX f m (runX f n a)
  Cat c -> f c a
  Pure b -> pure b
  Ap m n -> do
    func <- runX f n a
    val <- runX f m a
    return (func val)

interpretJ2 :: J2 a b -> MyError a -> MyError b
interpretJ2 x a = case x of
  J2A f -> f =<< a
  J2B -> case a of
    MyError e -> MyError (fmap (\x -> [x]) e)

interpret :: Free J2 a b -> a -> Either String [b]
interpret x a = getMyError (runX interpretJ2 x (pure a))
-- interpret x a = getMyError (run getJsonParse x a)


key :: Text -> Free J2 Aeson.Value Aeson.Value
key k = Cat $ J2A $ \x -> case x of
  Aeson.Object m -> case HashMap.lookup k m of
    Nothing -> MyError $ Left ("could not find key " ++ Text.unpack k)
    Just v -> MyError $ Right [v]
  _ -> MyError $ Left "expected Object"

string :: Free J2 Aeson.Value Text
string = Cat $ J2A $ \x -> case x of
  Aeson.String t -> MyError $ Right [t]
  _ -> MyError $ Left "expected String"

array :: Free J2 Aeson.Value [Aeson.Value]
array = Cat $ J2A $ \x -> case x of
  Aeson.Array a -> MyError $ Right [toList a]
  _ -> MyError $ Left "expected Array"

descend :: Free J2 [a] a
descend = Cat $ J2A (MyError . Right)

listify :: Free J2 a [a]
listify = Cat J2B

data Glossary = Glossary
  { glossaryAbbrev :: Text
  , glossaryAcronym :: Text
  , glossaryId :: Text
  , glossaryAlso :: [Text]
  } deriving (Show)

data Action = Action
  { actionName :: Text
  , actionLink :: Text
  } deriving (Show)

data From = From
  { fromName :: Text
  , fromId :: Text
  } deriving (Show)

ex2 :: Free J2 Aeson.Value Glossary
ex2 = id
  . ( Glossary
    <$> string . key "Abbrev"
    <*> string . key "Acronym"
    <*> string . key "ID"
    <*> listify . string . descend . array . key "GlossSeeAlso" . key "GlossDef"
    )
  . key "GlossEntry"
  . key "GlossList"
  . key "GlossDiv"
  . key "glossary"

ex3 :: Free J2 Aeson.Value (From,[Action])
ex3 = id
  . ( (,)
    <$>
      ( From
        <$> string . key "name"
        <*> string . key "id"
      ) . key "from"
    <*> ( listify .
      ( Action
        <$> string . key "name"
        <*> string . key "link"
      ) . descend . array . key "actions" )
    )
  . descend
  . array
  . key "data"


