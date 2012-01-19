{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.Cardinal
    ( Zero
    , Cardinal(..)

    , C0
    , C1
    , C2
    , C3
    , C4

    , Cardinality(..)
    , card

    , module Data.TypeAlgebra

    ) where

import Data.TypeAlgebra

data Zero

type C0 = Zero

type C1 = Succ C0

type C2 = Succ C1

type C3 = Succ C2

type C4 = Succ C3

class Cardinal a where
    -- | Cardinal number as a type. The associated data tyoe @'Succ'
    -- a@ provides the next cardinal type. The method @'fromCardinal'@
    -- provides a numeric representation of the cardinal number; it
    -- should be independent on the argument and work on
    -- @'undefined'@.
    data Succ a
    fromCardinal :: (Num i) => a -> i

instance Cardinal a => LEq a (Succ a)

instance Cardinal Zero where
    data Succ Zero
    fromCardinal _ = 0

instance Cardinal a => Cardinal (Succ a) where
    data Succ (Succ a)
    fromCardinal x = 1 + fromCardinal (p x)
        where p :: Succ a -> a
              p _ = undefined

instance Show Zero where
    show _ = "0"

instance Cardinal a => Show (Succ a) where
    show x = show (fromCardinal x :: Integer)

class Cardinal (Card a) => Cardinality a where
    -- | The cardinality of a type is defined by its @'Cardinal'@ type
    -- @'Card' a@.
    type Card a

-- | The numeric cardinality of a type. @'card'@ is independent on its
-- argument.
card :: (Cardinality a, Num i) => a -> i
card = fromCardinal . c
         where c :: a -> Card a
               c _ = undefined

instance Cardinality Zero where
    type Card Zero = Zero

instance Cardinality a => Cardinality (Succ a) where
    type Card (Succ a) = Succ (Card a)


instance Cardinal a => Sum a Zero where
    type a :+: Zero = a
    _ <+> _ = undefined

instance (Cardinal a, Cardinal b, Sum a b) => Sum a (Succ b) where
    type a :+: (Succ b) = Succ (a :+: b)
    _ <+> _ = undefined


instance Cardinal a => Prod a Zero where
    type a :*: Zero = Zero
    _ <*> _ = undefined

instance (Cardinal a, Prod a b) => Prod a (Succ b) where
    type a :*: (Succ b) = a :+: (a :*: b)
    _ <*> _ = undefined
