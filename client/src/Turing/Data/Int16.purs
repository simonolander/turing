module Turing.Data.Int16 (Int16) where

import Prelude

import Data.Newtype (class Newtype, wrap)

newtype Int16 = Int16 Int

derive instance newtypeInt16 :: Newtype Int16 _
derive instance eqInt16 :: Eq Int16
derive instance ordInt16 :: Ord Int16

instance showInt16 :: Show Int16 where
    show (Int16 a) = show a

instance semiringInt16 :: Semiring Int16 where
    add (Int16 a) (Int16 b) = wrap $ a + b
    mul (Int16 a) (Int16 b) = wrap $ a * b
    one = wrap 1
    zero = wrap 0

instance ringInt16 :: Ring Int16 where
    sub (Int16 a) (Int16 b) = wrap $ a - b
