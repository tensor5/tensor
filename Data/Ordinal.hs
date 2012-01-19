{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | In this module we provide a way to canonically define a totally
-- ordered set with a given number of elements.  These types have a
-- custom @'Show'@ instances so that their elements are displayed with
-- usual decimal number.
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
module Data.Ordinal
    ( One(..)
    , Succ(..)

    , Two
    , Three
    , Four
    , Five

    , Ordinal(..)

    , module Data.TypeAlgebra

    ) where

import           Data.Cardinal hiding (Succ)
import qualified Data.Cardinal as C
import           Data.Ord
import           Data.TypeAlgebra

-- | A set with one element.
data One = One
           deriving Eq

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

-- | If @n@ is a set with n elements, @'Succ' n@ is a set with n+1 elements.
data Succ n = First -- ^ The first element of the type.
            | Succ n -- ^ The last @n@ elements.
              deriving Eq

type Two = Succ One
type Three = Succ Two
type Four = Succ Three
type Five = Succ Four

instance (Bounded n) => Bounded (Succ n) where
    minBound = First
    maxBound = Succ (maxBound)


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

instance Monad Succ where
    First >>= _ = First
    (Succ x) >>= f = f x
    return x = Succ x

{-
class Cardinal n where
    -- | Number of elements inside the type @n@. In any instance of
    -- @'Cardinal'@ the method @'card'@ should be independent on the
    -- argument and work on @'undefined'@.
    card :: (Num i) => n -> i
-}
instance Cardinality One where
    type Card One = C.Succ Zero

instance Cardinality n => Cardinality (Succ n) where
    type Card (Succ n) = C.Succ (Card n)


-- | Class of ordered sets with n elements. The methods in this class
-- provide a convenient way to convert to and from a numeric type.
class (Cardinality n, Ord n) => Ordinal n where
    fromOrdinal :: (Num i) => n -> i
    toOrdinal :: (Num i) => i -> n

instance (Ordinal m) => Sum m One where
    type m :+: One = Succ m
    x <+> One = Succ x

instance (Ordinal m, Ordinal n, Ordinal (m :+: n), Sum m n) =>
    Sum m (Succ n) where
        type m :+: (Succ n) = Succ (m :+: n)
        x <+> First = toOrdinal (1 + fromOrdinal x :: Integer)
        x <+> (Succ y) = Succ (x <+> y)


instance (Ordinal m) => Prod m One where
    type m :*: One = m
    x <*> One = toOrdinal (fromOrdinal x :: Integer)

instance (Ordinal m, Ordinal n, Prod m n, Sum m (m :*: n), Ordinal (m :+: (m :*: n)))
    => Prod m (Succ n) where
        type m :*: (Succ n) = m :+: (m :*: n)
        x <*> First = toOrdinal (fromOrdinal x :: Integer)
        x <*> (Succ y) = x <+> (x <*> y)

{-
instance SubSet One (Succ One) where
    embed One = First

instance SubSet n (Succ n) => SubSet (Succ n)  (Succ (Succ n)) where
    embed First = First
    embed (Succ x) = Succ (embed x)
-}

--instance Ordinal n => SubSet n n where
--    embed x = toOrdinal (fromOrdinal x :: Integer)
{-
instance (SubSet m n, SubSet n (Succ n)) => SubSet m (Succ n) where
    embed x = let (a,b) = p undefined in
              asTypeOf (embed (asTypeOf (embed x) a)) b
        where p :: n -> (n, Succ n)
              p n = (n, Succ n)
-}
instance (Ordinal m, Ordinal n, SubSet m n) => SubSet m (Succ n) where
    embed x = toOrdinal (fromOrdinal x :: Integer)

instance Ordinal a => LEq a (Succ a)
