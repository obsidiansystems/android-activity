{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Cond where

import Data.Aeson (ToJSON(..), FromJSON(..))
import GHC.Generics

data Cond a = Cond_False
            | Cond_True
            | Cond_Atom a
            | Cond_Or [Cond a]
            | Cond_And [Cond a]
  deriving (Eq, Ord, Read, Show, Generic, Functor, Traversable, Foldable)

instance (Ord a, ToJSON a) => ToJSON (Cond a)
instance (Ord a, FromJSON a) => FromJSON (Cond a)

condOr :: [a] -> Cond a
condOr [] = Cond_False
condOr [x] = Cond_Atom x
condOr xs = Cond_Or (map Cond_Atom xs)

condAnd :: [a] -> Cond a
condAnd [] = Cond_True
condAnd [x] = Cond_Atom x
condAnd xs = Cond_And (map Cond_Atom xs)

foldCond :: b -> b -> (a -> b) -> ([b] -> b) -> ([b] -> b) -> Cond a -> b
foldCond false true atom orF andF = f
  where
    f Cond_False = false
    f Cond_True = true
    f (Cond_Atom x) = atom x
    f (Cond_Or xs) = orF (map f xs)
    f (Cond_And xs) = andF (map f xs)

evalCond :: Cond Bool -> Bool
evalCond = foldCond False True id or and