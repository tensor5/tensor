{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.MultiIndex where

import Data.HList
import Data.HList.FakePrelude
import Data.Ordinal

class (Cardinal a) ⇒ MultiIndex a where
    fromMultiIndex ∷ (Num n) ⇒ a → [n]
    toMultiIndex ∷ (Num n) ⇒ [n] → a
    dimensions ∷ (Num n) ⇒ a → [n]

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

instance (Ordinal a, MultiIndex b) ⇒ MultiIndex (a :|: b) where
    dimensions z = d (asTypeOf (undefined:|:undefined) z)
        where d (x :|: y) = (card x):(dimensions y)
    fromMultiIndex (x :|: y) = (fromOrdinal x):(fromMultiIndex y)
    toMultiIndex (x:xs) = (toOrdinal x) :|: (toMultiIndex xs)
    toMultiIndex [] = error "(toMultiIndex x): list too short"

instance (Ordinal a, MultiIndex b) ⇒ Show (a :|: b) where
    show (x :|: y) = show $ fromMultiIndex (x :|: y)

instance Bounded End where
    minBound = End
    maxBound = End

instance (Bounded a, Bounded b) ⇒ Bounded (a :|: b) where
    minBound = minBound :|: minBound
    maxBound = maxBound :|: maxBound

multiIndex2Linear ∷ (MultiIndex a, Num n) ⇒ a → n
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

{-
class MultiIndexJoin a b c d | a b c -> d where
    add :: b -> c -> d

instance (Ordinal a1, Ordinal a2) ⇒ MultiIndexJoin HNil (HCons a1 b) (HCons a2 b) (HCons (Sum a1 a2) b) where
    add (HCons x1 y) (HCons x2 y) = HCons (x1 ⨣ x2) y
-}

type Prod a b = (HAppend a b c) ⇒ c

class TakeUntil a b c | a b → c where
    takeUntil ∷ a → b → c

instance (MultiIndex a) ⇒ TakeUntil End a a where
    takeUntil _ x = x

instance (MultiIndex b, MultiIndex c, MultiIndex d, TakeUntil b c d) ⇒
    TakeUntil (a :|: b) (a :|: c) d where
    takeUntil (x :|: y) (x' :|: z) = takeUntil y z

class DropAt a b c | a b → c where
    dropAt ∷ a → b → c

class MReverse a b | a -> b, b -> a
 where
  mReverse:: a -> b

instance (HReverse' End a b, HReverse' End b c)
      =>  MReverse a b
 where
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