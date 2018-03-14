{-# LANGUAGE InstanceSigs #-}

module Nat
  ( Nat(..)
  , testNat
  ) where

data Nat = Z | S Nat

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  Z == Z          = True
  (S x) == (S y)  = x == y
  _ == _          = False

instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  Z <= _          = True
  _ <= Z          = False
  (S x) <= (S y)  = x <= y

instance Show Nat where
  show :: Nat -> String
  show = show . fromNat where
    fromNat :: Nat -> Int
    fromNat Z      = 0
    fromNat (S x)  = 1 + fromNat x

instance Num Nat where
  (+), (-), (*) :: Nat -> Nat -> Nat
  Z + x     = x
  (S x) + y = S (x + y)

  x - Z         = x
  Z - _         = error "Nat can not be negative"
  (S x) - (S y) = x - y

  Z * _ = Z
  (S x) * y = y + (x * y)

  signum :: Nat -> Nat
  signum Z = Z
  signum _ = S Z  

  abs :: Nat -> Nat
  abs = id

  fromInteger :: Integer -> Nat
  fromInteger 0 = Z
  fromInteger x
    | x < 0     = error "Nat can not be negative"
    | otherwise = S $ fromInteger (x - 1)

testNat :: Bool
testNat = fromInteger 10 + fromInteger 5 * fromInteger 2 - fromInteger 1 == fromInteger 19