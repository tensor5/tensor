{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | We define the a multidimensional array of indices called
-- @'MultiIndex'@. The canonical implementation of a @'MultiIndex'@ is
-- an heterogeneous list of @'Ordinal'@s.  Below we illustrate some
-- example of @'MultiIndex'@ types and the elements they contain.
--
-- @'Three' ':|:' 'End'@ = {(1),(2),(3)}
--
-- @'Three' ':|:' ('Two' ':|:' 'End')@ =
-- {(1,1),(1,2),(2,1),(2,2),(3,1),(3,2)}
--
-- @'Three' ':|:' ('Two' ':|:' ('Two' ':|:' 'End'))@ =
-- {(1,1,1),(1,1,2),(1,2,1),(1,2,2),(2,1,1),(2,1,2),(2,2,1),(2,2,2),(3,1,1),(3,1,2),(3,2,1),(3,2,2)}
module Data.MultiIndex where

import Data.HList
import Data.HList.FakePrelude
import Data.Ordinal

class (Cardinal i) ⇒ MultiIndex i where
    fromMultiIndex ∷ (Num n) ⇒ i → [n]
    toMultiIndex ∷ (Num n) ⇒ [n] → i
    dimensions ∷ (Num n) ⇒ i → [n]

-- | @'End'@ is used to signal the end of a list.
data End = End
           deriving (Eq)

instance MultiIndex End where
    dimensions _ = []
    fromMultiIndex End = []
    toMultiIndex [] = End
    toMultiIndex (x:xs) = error "(toMultiIndex x): list too long"

instance Show End where
    show End = show $ fromMultiIndex End

-- |This is the constructor for heterogeneous lists, equivalent to
-- @':'@ for standard lists. @'End'@ is used to end the lists, just
-- like @'[]'@. We don't use @'HCons'@ and @'HNil'@ because we want to
-- have a different @'Show'@ instance.
data a :|: b = a :|: b
               deriving Eq

instance HAppend End a a where
    hAppend End a = a

instance (HAppend a b c) ⇒ HAppend (d :|: a) b (d :|: c) where
    hAppend (x :|: y) z = x :|: (hAppend y z)

instance (Ordinal i, MultiIndex is) ⇒ MultiIndex (i :|: is) where
    dimensions z = d (asTypeOf (undefined:|:undefined) z)
        where d (x :|: y) = (card x):(dimensions y)
    fromMultiIndex (x :|: y) = (fromOrdinal x):(fromMultiIndex y)
    toMultiIndex (x:xs) = (toOrdinal x) :|: (toMultiIndex xs)
    toMultiIndex [] = error "(toMultiIndex x): list too short"

instance (Ordinal i, MultiIndex is) ⇒ Show (i :|: is) where
    show (x :|: y) = show $ fromMultiIndex (x :|: y)

instance Bounded End where
    minBound = End
    maxBound = End

instance (Bounded a, Bounded b) ⇒ Bounded (a :|: b) where
    minBound = minBound :|: minBound
    maxBound = maxBound :|: maxBound

multiIndex2Linear ∷ (MultiIndex i, Num n) ⇒ i → n
multiIndex2Linear i = fromM l d
    where fromM a b = case a of
                        [] → 1
                        x:[] → x
                        x:xs → ((head a) - 1) * (foldl1 (*) (tail b))
                               + (fromM (tail a) (tail b))
          l = fromMultiIndex i
          d = dimensions i

instance Cardinal End where
    card _ = 1

instance (Cardinal a, Cardinal b) ⇒ Cardinal (a :|: b) where
    card (x :|: y) = (card x) * (card y)

class MultiIndexConcat a b c where
    type Concat a b c

instance (Ordinal i1, Ordinal i2, Sum i1 i2, MultiIndex is)
    ⇒ MultiIndexConcat HZero (i1 :|: is) (i2 :|: is) where
        type Concat HZero (i1 :|: is) (i2 :|: is) = (Plus i1 i2) :|: is

instance (HNat n, Ordinal i, MultiIndex js, MultiIndex ks
         , MultiIndexConcat n js ks)
    ⇒ MultiIndexConcat (HSucc n) (i :|: js) (i :|: ks) where
        type Concat (HSucc n) (i :|: js) (i :|: ks) = (i :|: Concat n js ks)


type Prod a b = (HAppend a b c) ⇒ c

class TakeUntil a b c | a b → c where
    takeUntil ∷ a → b → c

instance (MultiIndex i) ⇒ TakeUntil End i i where
    takeUntil _ x = x

instance (MultiIndex is, MultiIndex js, MultiIndex ks, TakeUntil is js ks) ⇒
    TakeUntil (i :|: is) (i :|: js) ks where
    takeUntil (x :|: y) (x' :|: z) = takeUntil y z

class DropAt a b c | a b → c where
    dropAt ∷ a → b → c

class MReverse a b | a -> b, b -> a where
    mReverse :: a -> b

instance (HReverse' End a b, HReverse' End b a) ⇒ MReverse a b where
    mReverse x = hReverse' End x

instance HReverse' a End a where
    hReverse' x End = x

instance (HReverse' (a :|: b) c d) ⇒ HReverse' b (a :|: c) d where
    hReverse' x (a :|: y) = hReverse' (a :|: x) y

instance (MReverse a a', MReverse b b', MReverse c' c, TakeUntil a' b' c') ⇒
    DropAt a b c where
        dropAt x y = mReverse (takeUntil (mReverse x) (mReverse y))

instance HHead (a :|: b) a
 where
  hHead (x :|: _) = x