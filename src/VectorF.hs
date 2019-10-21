-- file src/VectorF.hs
--The following code is based on code in Data.Functor.Foldable, but applied to Vector instead of lists

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module VectorF where

import Data.Functor.Classes
import Data.Functor.Foldable 
import Data.Vector (Vector, cons, null, empty, head, tail, fromList, singleton, (++))
import qualified Data.Vector as V
import GHC.Generics
import Prelude hiding (head, tail, null, (++))

data VectorF a b = NilV | ConsV a b deriving (Show, Functor, Eq, Ord, Generic)

type instance Base (Vector a) = VectorF a
instance Recursive (Vector a) where
  project xs
    | null xs = NilV
    | otherwise = ConsV (head xs) (tail xs)
  para f xs
    | null xs = f NilV
    | otherwise = f (ConsV (head xs) (tail xs, para f (tail xs)))

instance Corecursive (Vector a) where
  embed (ConsV x xs) = x `cons` xs
  embed NilV = V.empty

  apo f a = case f a of
    ConsV x (Left xs) -> x `cons` xs
    ConsV x (Right b) -> x `cons` (apo f b)
    NilV -> V.empty

instance Eq2 VectorF where
  liftEq2 _ _ NilV        NilV          = True
  liftEq2 f g (ConsV a b) (ConsV a' b') = f a a' && g b b'
  liftEq2 _ _ _           _             = False

instance Eq a => Eq1 (VectorF a) where
  liftEq = liftEq2 (==)

