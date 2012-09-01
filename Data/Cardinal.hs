{-# LANGUAGE DeriveGeneric #-}
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
    , C5
    , C6
    , C7
    , C8
    , C9
    , C10

    , Cardinality(..)
    , card

    , GCardinality(..)

    , module Data.TypeAlgebra

    ) where

import           Data.TypeAlgebra
import           GHC.Generics hiding ((:+:), (:*:), C1)
import qualified GHC.Generics as G

data Zero
    deriving Generic

type C0 = Zero

type C1 = Succ C0

type C2 = Succ C1

type C3 = Succ C2

type C4 = Succ C3

type C5 = Succ C4

type C6 = Succ C5

type C7 = Succ C6

type C8 = Succ C7

type C9 = Succ C8

type C10 = Succ C9

-- | Cardinal number as a type. The associated data type @'Succ' a@
-- provides the next cardinal type. The method @'fromCardinal'@
-- provides a numeric representation of the cardinal number; it should
-- be independent on the argument and work on @'undefined'@.
class Cardinal a where
    data Succ a
    fromCardinal :: (Num i) => a -> i

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

-- | The cardinality of a type is defined by its @'Cardinal'@ type
-- @'Card' a@.
class Cardinal (Card a) => Cardinality a where
    type Card a

-- | The numeric cardinality of a type. @'card'@ is independent on its
-- argument.
card :: (Cardinality a, Num i) => a -> i
card = fromCardinal . c
         where c :: a -> Card a
               c _ = undefined

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


class GCardinality a where
    type GCard a

instance GCardinality (V1 p) where
    type GCard (V1 p) = Zero

instance GCardinality (U1 p) where
    type GCard (U1 p) = Succ Zero

instance Cardinality a => GCardinality (K1 i a p) where
    type GCard (K1 i a p) = Card a

instance GCardinality (f p) => GCardinality (M1 i c f p) where
    type GCard (M1 i c f p) = GCard (f p)

instance (GCardinality (f p), GCardinality (g p)) => GCardinality ((f G.:+: g) p) where
    type GCard ((f G.:+: g) p) = (GCard (f p)) :+: (GCard (g p))

instance (GCardinality (f p), GCardinality (g p)) => GCardinality ((f G.:*: g) p) where
    type GCard ((f G.:*: g) p) = (GCard (f p)) :*: (GCard (g p))

