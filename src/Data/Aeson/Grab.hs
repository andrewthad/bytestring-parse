{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Grab where

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

newtype Grab a b = Grab { runGrab :: a -> [b] }
  deriving (Functor)

instance C.Category Grab where
  id = Grab (\a -> [a])
  Grab f . Grab g = Grab (f <=< g)

instance Profunctor Grab where
  dimap f g (Grab h) = Grab (map g . h . f)

instance Strong Grab where
  first' (Grab f) = Grab (\(a,c) -> zip (f a) (repeat c))

key :: Text -> Grab Aeson.Value Aeson.Value
key k = Grab $ \x -> case x of
  Aeson.Object m -> maybeToList (HashMap.lookup k m)
  _ -> []

string :: Grab Aeson.Value Text
string = Grab $ \x -> case x of
  Aeson.String t -> [t]
  _ -> []

number :: Grab Aeson.Value Scientific
number = Grab $ \x -> case x of
  Aeson.Number n -> [n]
  _ -> []

array :: Grab Aeson.Value Aeson.Value
array = Grab $ \x -> case x of
  Aeson.Array xs -> toList xs
  _ -> []

dup :: Grab a (a,a) 
dup = Grab (\a -> [(a,a)])

ex1 :: Grab Aeson.Value (Text,Text)
ex1 = id
  . second'
  ( string
  . key "Abbrev"
  )
  . first' 
  ( string
  . array
  . key "GlossSeeAlso"
  . key "GlossDef"
  )
  . dup
  . key "GlossEntry"
  . key "GlossList"
  . key "GlossDiv"
  . key "glossary"


