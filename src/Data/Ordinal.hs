{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  © 2012-2013 Nicola Squartini
-- License     :  GPL-3
--
-- Maintainer  :  Nicola Squartini <tensor5@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- In this module we provide a way to canonically define a totally ordered set
-- with a given number of elements.  These types have a custom @'Show'@
-- instances so that their elements are displayed with usual decimal number.
--
--
-- @'One'@ = {'One'} = {1}
--
-- @'Succ' 'One'@ = {@'First'@, @'Succ' 'One'@} = {1,2}
--
-- @'Succ' 'Succ' 'One'@ = {@'First'@, @'Succ' 'First'@, @'Succ'
-- 'Succ' 'One'@} = {1,2,3}
--
-- ...
--
--------------------------------------------------------------------------------

module Data.Ordinal
    ( One(..)
    , Succ(..)

    , Two
    , Three
    , Four
    , Five
    , Six
    , Seven
    , Eight
    , Nine
    , Ten

    , Ordinal(..)

    , module Data.TypeAlgebra

    ) where

import           Control.Applicative as A
import           Data.Cardinal    hiding (Succ)
import qualified Data.Cardinal    as C
import           Data.Ord         ()
import           Data.TypeAlgebra
import           Data.TypeAlgebra as TA
import qualified GHC.Generics     as G
import           System.Random

-- | A set with one element.
data One = One
           deriving (Eq, G.Generic)

instance Bounded One where
    minBound = One
    maxBound = One


instance Ord One where
    compare One One = EQ


instance Ordinal One where
    fromOrdinal One = 1
    toOrdinal 1 = One
    toOrdinal _ = error "(toOrdinal n): n is out of bounds"

instance Enum One where
    succ _ = error "Prelude.Enum.One.succ: bad argument"
    pred _ = error "Prelude.Enum.One.succ: bad argument"
    toEnum = toOrdinal
    fromEnum = fromOrdinal
    enumFrom _ = [One]
    enumFromThen _ _ = [One]
    enumFromTo _ _ = [One]
    enumFromThenTo _ _ _ = [One]

instance Show One where
    show One = "1"

instance Random One where
    randomR _ g = (One, snd $ next g)
    random g = (One, snd $ next g)

-- | If @n@ is a set with n elements, @'Succ' n@ is a set with n+1 elements.
data Succ n = First -- ^ The first element of the type.
            | Succ n -- ^ The last @n@ elements.
              deriving (Eq, G.Generic)

type Two = Succ One
type Three = Succ Two
type Four = Succ Three
type Five = Succ Four
type Six = Succ Five
type Seven = Succ Six
type Eight = Succ Seven
type Nine = Succ Eight
type Ten = Succ Nine

instance (Bounded n) => Bounded (Succ n) where
    minBound = First
    maxBound = Succ maxBound


instance (Ord n) => Ord (Succ n) where
    compare (Succ x) (Succ y) = compare x y
    compare (Succ _) First = GT
    compare First (Succ _) = LT
    compare First First = EQ


instance (Ordinal n) => Ordinal (Succ n) where
    fromOrdinal First = 1
    fromOrdinal (Succ x) = 1 + fromOrdinal x
    toOrdinal x | x == 1 = First
                | otherwise = Succ (toOrdinal (x - 1))

instance (Bounded n, Ordinal n) => Random (Succ n) where
    randomR (l,h) g =
        let (r,g') = randomR (fromOrdinal l :: Integer, fromOrdinal h) g
        in (toOrdinal r, g')
    random = randomR (minBound,maxBound)

instance (Bounded n, Enum n, Ordinal n) => Enum (Succ n) where
    succ First = Succ minBound
    succ (Succ n) = Succ (succ n)
    toEnum = toOrdinal
    fromEnum = fromOrdinal
    enumFrom     x   = enumFromTo     x maxBound
    enumFromThen x y = enumFromThenTo x y bound
        where
          bound | fromEnum y >= fromEnum x = maxBound
                | otherwise                = minBound


instance (Ordinal n) => Show (Succ n) where
    show x = show (fromOrdinal x :: Integer)

instance Functor Succ where
    fmap _ First = First
    fmap f (Succ x) = Succ (f x)

instance A.Applicative Succ where
    pure = return
    (Succ f) <*> (Succ x) = Succ (f x)
    _        <*> _        = First

instance Monad Succ where
    First >>= _ = First
    (Succ x) >>= f = f x
    return = Succ


instance Cardinality One where
    type Card One = C.Succ Zero

instance Cardinality n => Cardinality (Succ n) where
    type Card (Succ n) = C.Succ (Card n)


-- | Class of ordered sets with n elements. The methods in this class
-- provide a convenient way to convert to and from a numeric type.
class (Cardinality n, Ord n) => Ordinal n where
    fromOrdinal :: (Num i) => n -> i
    toOrdinal :: (Eq i, Num i) => i -> n

instance (Ordinal m) => Sum m One where
    type m :+: One = Succ m
    x <+> One = Succ x

instance (Ordinal m, Ordinal n, Ordinal (m :+: n), Sum m n) =>
    Sum m (Succ n) where
        type m :+: Succ n = Succ (m :+: n)
        x <+> First = toOrdinal (1 + fromOrdinal x :: Integer)
        x <+> (Succ y) = Succ (x <+> y)


instance (Ordinal m) => Prod m One where
    type m :*: One = m
    x <*> One = toOrdinal (fromOrdinal x :: Integer)

instance (Ordinal m, Ordinal n, Prod m n, Sum m (m :*: n), Ordinal (m :+: (m :*: n)))
    => Prod m (Succ n) where
        type m :*: Succ n = m :+: (m :*: n)
        x <*> First = toOrdinal (fromOrdinal x :: Integer)
        x <*> (Succ y) = x <+> (x TA.<*> y)

