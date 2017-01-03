{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Data.Aeson.Grab2 where

import Prelude hiding ((.),id)
import Control.Category as C
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Data.Profunctor
import Data.Profunctor.Strong
import Data.Text (Text)
import Data.Maybe
import Data.Scientific (Scientific)
import Data.Foldable

data Grab a b where
  GrabPure :: b -> Grab a b
  GrabAp :: Grab c a -> Grab c (a -> b) -> Grab c b
  GrabCompose :: Grab b c -> Grab a b -> Grab a c
  GrabOne :: (a -> Maybe b) -> Grab a b
  -- GrabFlatten :: Grab a [[b]] -> Grab a [b]

instance C.Category Grab where
  id = GrabOne Just
  g . f = GrabCompose g f

instance Functor (Grab a) where
  fmap f x = case x of
    GrabPure b -> GrabPure (f b)
    GrabOne g -> GrabOne (fmap f . g)
    GrabCompose m n -> GrabCompose (fmap f m) n
    GrabAp a y -> GrabAp a (fmap (f .) y)
    -- GrabFlatten g -> GrabFlatten (_ g)

instance Applicative (Grab a) where
  pure b = GrabPure b
  x <*> y = GrabAp y x

runGrab :: Grab a b -> a -> Maybe b
runGrab x a = case x of
  GrabPure b -> Just b
  GrabOne f -> f a
  GrabAp g f -> do
    func <- runGrab f a 
    val <- runGrab g a
    return (func val)
  GrabCompose f g -> runGrab f =<< runGrab g a
  -- GrabFlatten g -> fmap join (runGrab g a)

key :: Text -> Grab Aeson.Value Aeson.Value
key k = GrabOne $ \x -> case x of
  Aeson.Object m -> HashMap.lookup k m
  _ -> Nothing

string :: Grab Aeson.Value Text
string = GrabOne $ \x -> case x of
  Aeson.String t -> Just t
  _ -> Nothing

array :: Grab Aeson.Value [Aeson.Value]
array = GrabOne $ \x -> case x of
  Aeson.Array xs -> Just (toList xs)
  _ -> Nothing

-- mapGrab :: Grab a b -> Grab [a] [b]
-- mapGrab x = case x of
--   GrabPure a -> GrabPure [a]
--   GrabOne f -> GrabOne (\a -> mapM f a)
--   GrabCompose m n -> GrabCompose (mapGrab m) (mapGrab n)
--   GrabAp a f -> GrabAp (mapGrab a) (fmap map f)
-- 
-- helpMap :: Grab c (a -> b) -> Grab [c] ([a] -> [b])
-- helpMap x = case x of
--   GrabAp a f -> GrabAp (mapGrab a) (fmap map f)

joinGrab :: Grab [[b]] [b] 
joinGrab = GrabAp C.id (GrabPure join) 

ex1 :: Grab Aeson.Value Text
ex1 = id
  . string
  . key "Abbrev"
  . key "GlossEntry"
  . key "GlossList"
  . key "GlossDiv"
  . key "glossary"

data Glossary = Glossary
  { glossaryAbbrev :: Text
  , glossaryAcronym :: Text
  , glossaryId :: Text
  } deriving (Show)

ex2 :: Grab Aeson.Value Glossary
ex2 = id
  . ( Glossary
    <$> string . key "Abbrev"
    <*> string . key "Acronym"
    <*> string . key "ID"
    )
  . key "GlossEntry"
  . key "GlossList"
  . key "GlossDiv"
  . key "glossary"

-- ex3 :: Grab Aeson.Value [Text]
-- ex3 = id
--   . mapGrab 
--     ( string . key "name"
--     )
--   . joinGrab
--   . mapGrab
--     ( array
--     . key "actions"
--     )
--   . array
--   . key "data"


