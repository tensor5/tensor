{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | We define the a multidimensional array of indices called
-- @'MultiIndex'@. The canonical implementation of a @'MultiIndex'@ is
-- an heterogeneous list of @'Ordinal'@s.  Below we illustrate some
-- example of @'MultiIndex'@ types and the elements they contain.
--
-- @'Three' ':|:' 'Nil'@ = {(1),(2),(3)}
--
-- @'Three' ':|:' ('Two' ':|:' 'Nil')@ =
-- {(1,1),(1,2),(2,1),(2,2),(3,1),(3,2)}
--
-- @'Three' ':|:' ('Two' ':|:' ('Two' ':|:' 'Nil'))@ =
-- {(1,1,1),(1,1,2),(1,2,1),(1,2,2),(2,1,1),(2,1,2),(2,2,1),(2,2,2),(3,1,1),(3,1,2),(3,2,1),(3,2,2)}

module Data.TypeList.MultiIndex
    ( Nil(..)
    , (:|:)(..)

    , module Data.Ordinal
    , module Data.TypeList

    , MultiIndex(..)
    , multiIndex2Linear
    , Dimensions(..)
    , MultiIndexConcat(..)

    ) where

import           Data.Cardinal hiding (Succ)
import qualified Data.Cardinal as C
import           Data.Ordinal
import           Data.TypeList
import           Data.TypeList.MultiIndex.Internal
import qualified GHC.Generics as G
import           Prelude hiding (drop, take)
import           System.Random


data Nil = Nil
           deriving (Eq, G.Generic)


-- |This is the constructor for heterogeneous lists, equivalent to
-- @':'@ for standard lists. @'Nil'@ is used to end the lists, just
-- like @'[]'@.
data a :|: b = a :|: b
                deriving (Eq, G.Generic)

infixr 9 :|:

instance Cardinality Nil where
    type Card Nil = C1

instance (Cardinality e, Cardinality l, Cardinal ((Card e) :*: (Card l))) =>
    Cardinality (e :|: l) where
        type Card (e :|: l) = (Card e) :*: (Card l)


instance TypeList Nil where
    type Length Nil = C0

instance TypeList l => TypeList (e :|: l) where
    type Length (e :|: l) = C.Succ (Length l)


instance TypeList l => HeadTail (e :|: l) where
    type Head (e :|: l) = e
    type Tail (e :|: l) = l
    head (e :|: _) = e
    tail (_ :|: l) = l
    e .|. l = e :|: l


instance TypeList l => AppendList Nil l where
    type Nil :++: l = l
    Nil <++> l = l

instance (AppendList l l') => AppendList (e :|: l) l' where
    type (e :|: l) :++: l' = e :|: (l :++: l')
    (e :|: l) <++> l' = e :|: (l <++> l')


instance TakeList Zero Nil where
    type Take Zero Nil = Nil
    take _ _ = Nil

instance TakeList Zero l => TakeList Zero (e :|: l) where
    type Take Zero (e :|: l) = Nil
    take _ _ = Nil

instance TakeList n l => TakeList (C.Succ n) (e :|: l) where
    type Take (C.Succ n) (e :|: l) = e :|: (Take n l)
    take x (e :|: l) = e :|: (take (p x) l)
        where p :: C.Succ n -> n
              p _ = undefined


instance DropList Zero Nil where
    type Drop Zero Nil = Nil
    drop _ = id

instance DropList Zero l => DropList Zero (e :|: l) where
    type Drop Zero (e :|: l) = (e :|: l)
    drop _ = id

instance DropList n l => DropList (C.Succ n) (e :|: l) where
    type Drop (C.Succ n) (e :|: l) = Drop n l
    drop x (_ :|: l) = drop (p x) l
        where p :: C.Succ n -> n
              p _ = undefined


instance TypeList l => TailRevList Nil l where
    type TailRev Nil l = l
    rev _ = id

instance (TailRevList l (e :|: l'), TypeList l') =>
    TailRevList (e :|: l) l' where
        type TailRev (e :|: l) l' = TailRev l (e :|: l')
        rev (e :|: l) l' = rev l (e :|: l')


instance ReverseList Nil where
    type Reverse Nil = Nil
    reverse l = l

instance (TailRevList l Nil, TailRevList (e :|: l) Nil) =>
    ReverseList (e :|: l) where
        type Reverse (e :|: l) = TailRev (e :|: l) Nil
        reverse l = rev l Nil


class (Dimensions i, TypeList i) => MultiIndex i where
    fromMultiIndex :: (Num n) => i -> [n]
    toMultiIndex :: (Eq n, Num n) => [n] -> i

instance MultiIndex Nil where
    fromMultiIndex Nil = []
    toMultiIndex [] = Nil
    toMultiIndex (_:_) = error "(toMultiIndex x): list too long"

instance Show Nil where
    show x = show (fromMultiIndex x :: [Integer])

instance (Ordinal i, MultiIndex is) => MultiIndex (i :|: is) where
    fromMultiIndex (x :|: y) = (fromOrdinal x):(fromMultiIndex y)
    toMultiIndex (x:xs) = (toOrdinal x) :|: (toMultiIndex xs)
    toMultiIndex [] = error "(toMultiIndex x): list too short"

instance (Ordinal i, MultiIndex is) => Show (i :|: is) where
    show x = show (fromMultiIndex x :: [Integer])


multiIndex2Linear :: (MultiIndex i, Num n) => i -> n
multiIndex2Linear i = linearize d l
    where l = fromMultiIndex i
          d = dimensions i


-- | Class for types having multiple dimensions, like @'MultiIndex'@es
-- or @'Tensor'@s.
class Dimensions i where
    -- | Returns the dimensions list. It should always be independent
    -- on its argument and work on @'undefined'@.
    dimensions :: (Num n) => i -> [n]

instance Dimensions Nil where
    dimensions _ = []

instance (Ordinal i, Dimensions is) => Dimensions (i :|: is) where
    dimensions z = d (asTypeOf (undefined:|:undefined) z)
        where d (x :|: y) = (card x):(dimensions y)


class (Cardinal n, MultiIndex is, MultiIndex js) =>
    MultiIndexConcat n is js where
        type Concat n is js

instance (Ordinal i1, Ordinal i2, Sum i1 i2, MultiIndex is) =>
    MultiIndexConcat Zero  (i1 :|: is) (i2 :|: is) where
        type Concat Zero (i1 :|: is) (i2 :|: is) = (i1 :+: i2) :|: is

instance (Cardinal n, Ordinal i, MultiIndex js, MultiIndex ks
         , MultiIndexConcat n js ks)
    => MultiIndexConcat (C.Succ n) (i :|: js) (i :|: ks) where
        type Concat (C.Succ n) (i :|: js) (i :|: ks) = (i :|: Concat n js ks)


instance Bounded Nil where
    minBound = Nil
    maxBound = Nil

instance Random Nil where
    randomR _ g = (Nil, snd $ next g)
    random g = (Nil, snd $ next g)

instance (Bounded e, Bounded l) => Bounded (e :|: l) where
    minBound = minBound :|: minBound
    maxBound = maxBound :|: maxBound

instance (Random e, Random l) => Random (e :|: l) where
    randomR (e :|: l, e' :|: h) g = let (a, g') = randomR (e, e') g
                                        (b, g'') = randomR (l, h) g'
                                    in (a :|: b, g'')
    random g = let (a, g') = random g
                   (b, g'') = random g'
               in (a :|: b, g'')

instance Extend Nil l' where
    type Ext Nil l' = l'
    extend _ l' = l'

instance Extend l l' => Extend (e :|: l) (e :|: l') where
    type Ext (e :|: l) (e :|: l') = Ext l l'
    extend (e :|: l) k = e :|: extend l k

