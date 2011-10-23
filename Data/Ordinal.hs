{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ordinal where

import Data.Ord

-- | Class of ordered sets with n elements.
class (Cardinal n, Ord n) => Ordinal n where
--    next :: n -> Succ n
    fromOrdinal :: (Num i) => n -> i
    toOrdinal :: (Num i) => i -> n


-- | A set with one element.
data One = One
           deriving Eq

instance Bounded One where
    minBound = One
    maxBound = One


instance Ord One where
    compare One One = EQ


instance Ordinal One where
--    next One = Succ One
    fromOrdinal One = 1
    toOrdinal 1 = One
    toOrdinal _ = error "(toOrdinal n): n is out of bounds"

instance Show One where
    show _ = "1"

-- | If @n@ is a set with n elements, @'Succ' n@ is a set with n+1 elements.
data Succ n = First
            | Succ n
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


instance (Ordinal n) => Ordinal (Succ n) where
--    next First = Succ First
--    next (Succ x) = Succ (next x)
    fromOrdinal First = 1
    fromOrdinal (Succ x) = 1 + fromOrdinal x
    toOrdinal x | x == 1 = First
                | otherwise = Succ (toOrdinal (x - 1))

instance (Ordinal n) => Show (Succ n) where
    show x = show (fromOrdinal x)

instance Functor Succ where
    fmap f First = First
    fmap f (Succ x) = Succ (f x)

instance Monad Succ where
    First >>= f = First
    (Succ x) >>= f = f x
    return x = Succ x

-- | In any instance of @'Cardinal'@ the method @'card'@ should be
-- independent on the argument and work on @'undefined'@.
class Cardinal n where
    card :: (Num i) => n -> i

instance Cardinal One where
    card _ = 1

instance (Cardinal a) => Cardinal (Succ a)  where
    card x = 1 + card (p x)
             where p :: Succ n -> n
                   p _ = undefined

class Sum a b where
    type Plus a b
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

