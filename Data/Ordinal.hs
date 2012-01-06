{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
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
module Data.Ordinal where

import Data.Ord

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
    show _ = "1"

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
    compare (Succ x) First = GT
    compare First (Succ y) = LT
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
    show x = show (fromOrdinal x)

instance Functor Succ where
    fmap f First = First
    fmap f (Succ x) = Succ (f x)

instance Monad Succ where
    First >>= f = First
    (Succ x) >>= f = f x
    return x = Succ x


class Cardinal n where
    -- | Number of elements inside the type @n@. In any instance of
    -- @'Cardinal'@ the method @'card'@ should be independent on the
    -- argument and work on @'undefined'@.
    card :: (Num i) => n -> i

instance Cardinal One where
    card _ = 1

instance (Cardinal a) => Cardinal (Succ a)  where
    card x = 1 + card (p x)
             where p :: Succ n -> n
                   p _ = undefined


-- | Class of ordered sets with n elements. The methods in this class
-- provide a convenient way to convert to and from a numeric type.
class (Cardinal n, Ord n) => Ordinal n where
    fromOrdinal :: (Num i) => n -> i
    toOrdinal :: (Num i) => i -> n


-- | Sum of types.
class Sum a b where
    type Plus a b
    -- | The sum of an element of @a@ and an element of @b@ is an
    -- element in the type @'Plus' a b@.
    (<+>) :: a -> b -> Plus a b

instance Sum m One where
    type Plus m One = Succ m
    x <+> One = Succ x

instance (Ordinal m, Sum m n, Ordinal (Plus m n)) => Sum m (Succ n) where
    type Plus m (Succ n) = Succ (Plus m n)
    x <+> First = toOrdinal (1 + fromOrdinal x)
    x <+> (Succ y) = Succ (x <+> y)


-- | The type @a@ is @'Next'@ to @b@ if @a@ can be canonically
-- embedded into @b@.
class Next a b | a -> b where
    embed :: a -> b

instance Next One (Succ One) where
    embed One = First

instance (Next n (Succ n)) => Next (Succ n) (Succ (Succ n)) where
    embed First = First
    embed (Succ x) = Succ (embed x)

