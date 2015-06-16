{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE Trustworthy            #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  © 2012-2015 Nicola Squartini
-- License     :  BSD3
--
-- Maintainer  :  Nicola Squartini <tensor5@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- We call a /tensor/ a multi dimensional array with fixed non-zero
-- dimensions. This module defines a standard implementation of such an array
-- called @'Tensor'@, plus a class @'IsTensor'@ of types isomorphic to
-- @'Tensor'@ that can be used to define more efficient custom implementations.
--
-- In this and other modules we use the following notation:
--
--   1. Dimension of a tensor: the total number of elements in the
--   array. Ususally denoted with the letter @d@.
--
--   2. Rank of a tensor: the dimension of the array; this is the same as the
--   length of the multiindex. Usually denoted with the letter @r@.
--
--   3. In a tensor of rank @r@ the sizes of the array are denoted with the
--   letter @d@ and a subscript, for example a tensor with sizes @[d₁,…,dᵣ]@ has
--   dimension @d₁⋅…⋅dᵣ@.
--
--------------------------------------------------------------------------------

module Data.Tensor where

import           Control.Applicative
import           Control.Arrow
import           Control.DeepSeq
import           Control.Exception            (Exception)
import           Control.Exception            (throw)
import           Data.Singletons
import           Data.Singletons.Prelude.List hiding (Reverse)
import           Data.Typeable
import           GHC.Exts                     (IsList (..))
import           Prelude                      hiding (concat, map)
import           Prelude.Unicode
import           System.Random                hiding (split)

import           Data.Indexable
import           Data.MultiIndex              hiding (fromList, toList)
import           Data.Sliceable               hiding (AllCons, NilS, Slicer)
import qualified Data.Sliceable               as S


-- | Class of types isomorphic to @'Tensor'@.
class ( IsMultiIndex (Index t)
      , MultiIndexable t
      ) ⇒ IsTensor (t ∷ [PI] → * → *) where
    t1 ∷ t is e → t ('One ': is) e
    unT1 ∷ t ('One ': is) e → t is e
    (|:) ∷ t is e → t (n ': is) e → t ('S n ': is) e
    unCons ∷ t ('S n ': is) e → (t is e, t (n ': is) e)
    fromTensor ∷ Tensor is e → t is e
    fromTensor (T0 e)    = t0 e
    fromTensor (T1 t)    = t1 $ fromTensor t
    fromTensor (t :| ts) = fromTensor t |: fromTensor ts
    toTensor ∷ SingI is ⇒ t is e → Tensor is e
    toTensor = toT sing
        where toT ∷ Shape is → t is e → Tensor is e
              toT SNil              = T0 ∘ unT0
              toT (SCons SOne   sh) = T1 ∘ toT sh ∘ unT1
              toT (SCons (SS i) sh) =
                  uncurry (:|) ∘ (toT sh *** toT (SCons i sh)) ∘ unCons
    -- |
    --
    -- @'split' n ∘ 'append' n ≡ '(,)'@
    append ∷ AppendI n is js ks ⇒ SPI n → t is e → t js e → t ks e
    -- |
    --
    -- @'uncurry' ('append' n) ∘ 'split' n ≡ 'id'@
    split ∷ AppendI n is js ks ⇒ SPI n → t ks e → (t is e, t js e)


-- | When a tensor is made instance of @'IsList'@ this @'Exception'@ should be
-- used by the @'fromList'@ method to signal an unappropriate list argument.
data TensorException = WrongListLength
                       deriving (Eq, Show, Typeable)

-- | Default instance.
instance Exception TensorException

-- | Transform a vector into a one-row matrix.
vector2RowVector ∷ IsTensor t ⇒ t '[i] e → t '[ 'One, i] e
vector2RowVector = t1

-- | Transform a one-row matrix into a vector.
rowVector2Vector ∷ IsTensor t ⇒ t '[ 'One, i] e → t '[i] e
rowVector2Vector = unT1


-- | Expresses the possible ways to append one tensor to another.
data Append ∷ PI → [PI] → [PI] → [PI] → * where
    A1 ∷ Append 'One ('One ': is) (i ': is) ('S i ': is)
    A1S ∷ Append 'One (i ': is) (j ': is) (k ': is)
        → Append 'One ('S i ': is) (j ': is) ('S k ': is)
    An ∷ Append n is js ks → Append ('S n) (i ': is) (i ': js) (i ': ks)

-- | Class for implicit @'Append'@ parameter.
class AppendI (n ∷ PI) (is ∷ [PI]) (js ∷ [PI]) (ks ∷ [PI]) | n is js → ks where
    appendSing ∷ Append n is js ks

-- | Appending @1 : is@ and @i : is@ at the first component results in @(i+1) :
-- is@ (@'appendSing' = 'A1'@).
instance AppendI 'One ('One ': is) (i ': is) ('S i ': is) where
    appendSing = A1

-- | To append @(i+1) : is@ and @j : is@ at the first component, append @i : is@
-- and @j : is@ and increase the first component by 1 (@'appendSing' = 'A1S'
-- 'appendSing'@).
instance AppendI 'One (i ': is) (j ': is) (k ': is) ⇒
    AppendI 'One ('S i ': is) (j ': is) ('S k ': is) where
    appendSing = A1S appendSing

-- | To append @i : is@ and @i : js@ at the (n+1)-th component, append @is@ and
-- @js@ at the n-th component and cons @i@ (@'appendSing' = 'An' 'appendSing'@).
instance AppendI n is js ks ⇒ AppendI ('S n) (i ': is) (i ': js) (i ': ks) where
    appendSing = An appendSing

--------------------------------------------------------------------------------

-- | A multi dimensional array with fixed non-zero dimensions.
data Tensor ∷ [PI] → * → * where
    T0   ∷ e → Tensor '[] e
    T1   ∷ Tensor is e → Tensor ('One ': is) e
    (:|) ∷ Tensor is e → Tensor (n ': is) e → Tensor ('S n ': is) e

infixr 5 :|

-- | A vector.
type Vector i = Tensor '[i]

-- | A matrix.
type Matrix i j = Tensor '[i, j]

-- | A matrix with only one column.
type ColumnVector i = Matrix i 'One

-- | A matrix with only one row.
type RowVector i = Matrix 'One i

-------------------------------------  Eq  -------------------------------------

-- | Standard equality.
instance Eq e ⇒ Eq (Tensor is e) where
    T0 e      == T0 f      = e ≡ f
    T1 t      == T1 u      = t ≡ u
    (t :| ts) == (u :| us) = t ≡ u ∧ ts ≡ us

-----------------------------------  Functor -----------------------------------

-- | @'fmap' = 'map'@.
instance Functor (Tensor is) where
    fmap = map

---------------------------------  Applicative ---------------------------------

-- | @'pure' a@ yields a @'Tensor'@ with all components equal to @a@. @'<*>'@
-- applies the @'Tensor'@ of functions componentwise.
instance SingI is ⇒ Applicative (Tensor is) where
    pure = p sing
        where p ∷ Shape js → e → Tensor js e
              p SNil              = T0
              p (SCons SOne   sh) = T1 ∘ p sh
              p (SCons (SS i) sh) = uncurry (:|) ∘ (p sh &&& p (SCons i sh))
    (<*>) = ap

------------------------------------  Show  ------------------------------------

-- | Rank 0 @'Tensor'@s are shown as a single element, rank 1 as lists, rank 2
-- as lists of lists, and so on, using [row-major
-- order](http://en.wikipedia.org/wiki/Row-major_order).
instance Show e ⇒ Show (Tensor is e) where
    showsPrec n (T0 e)    = showsPrec n e
    showsPrec n (T1 t)    = ('[':) ∘ showsPrec n t ∘ (']':)
    showsPrec n (t :| ts) =
        ('[':) ∘ showsPrec n t ∘ (',':) ∘ tail ∘ showsPrec n ts

----------------------------------  Indexable ----------------------------------

-- | @('!')@ has @O(d)@ complexity.
instance Indexable Tensor where
    type Index Tensor = MultiIndex
    T0 e      ! _           = e
    T1 t      ! OneCons is  = t ! is
    (t :| _)  ! OneCons is  = t ! is
    (_ :| ts) ! HeadSucc is = ts ! is
    generateA = genA sing
        where
          genA ∷ Applicative f ⇒ Shape is → (MultiIndex is → f e) → f (Tensor is e)
          genA SNil              f = liftA T0 $ f Nil
          genA (SCons SOne   is) f = liftA T1 $ genA is (f ∘ OneCons)
          genA (SCons (SS i) is) f =
              liftA2 (:|) (genA is (f ∘ OneCons))
                          (genA (SCons i is) (f ∘ HeadSucc))
    map f (T0 e)    = T0 $ f e
    map f (T1 t)    = T1 $ map f t
    map f (t :| ts) = map f t :| map f ts
    ap (T0 f)    (T0 e)    = T0 (f e)
    ap (T1 f)    (T1 t)    = T1 $ ap f t
    ap (f :| fs) (t :| ts) = ap f t :| ap fs ts

-------------------------------  MultiIndexable  -------------------------------

-- | @'t0' = 'T0'@, @'unT0' ('T0' e) = e@. Other methods are recursively
-- defined.
instance MultiIndexable Tensor where
    t0 = T0
    unT0 (T0 e) = e
    concat (T1 t)    = map T1 t
    concat (t :| ts) = (:|) `map` t `ap` (concat ts)
    unConcat (T0 (T1 t))    = T1 $ T0 t
    unConcat (T0 (t :| ts)) = T0 t :| unConcat (T0 ts)
    unConcat (T1 t)         = putT1 $ unConcat t
        where putT1 ∷ Tensor (i ': is) e → Tensor (i ': 'One ': is) e
              putT1 (T1 u)     = T1 $ T1 u
              putT1 (u :| us) = T1 u :| putT1 us
    unConcat (t :| ts)      = unConcat t |:: unConcat ts
        where (|::) ∷ Tensor (i ': is) e
                    → Tensor (i ': j ': is) e
                    → Tensor (i ': 'S j ': is) e
              T1 u      |:: T1 v      = T1 (u :| v)
              (u :| us) |:: (v :| vs) = (u :| v) :| (us |:: vs)
    T1 t      `at` is = T1 $ T0 (t ! is)
    (t :| ts) `at` is = T0 (t ! is) :| (ts `at` is)
    OneCons _   `ta` T1 t      = t
    OneCons _   `ta` (t :| _)  = t
    HeadSucc is `ta` (_ :| ts) = is `ta` ts
    rev = rev' reverseSing
        where rev' ∷ Reverse is js ks → Tensor is (Tensor js e) → Tensor ks e
              rev' R0     = unT0
              rev' (R1 r) = rev' r ∘ concat
    unRev = unRev' reverseSing
        where unRev' ∷ Reverse is js ks → Tensor js (Tensor is e) → Tensor ks e
              unRev' R0     = fmap unT0
              unRev' (R1 r) = unRev' r ∘ unConcat

----------------------------------  IsTensor  ----------------------------------

-- | Trivial instance (@'fromTensor' = 'id'@, @'toTensor' = 'id'@).
instance IsTensor Tensor where
    t1 = T1
    unT1 (T1 t) = t
    (|:) = (:|)
    unCons (t :| ts) = (t, ts)
    fromTensor = id
    toTensor = id
    append = append' appendSing
        where append' ∷ Append n is js ks
                      → SPI n → Tensor is e → Tensor js e → Tensor ks e
              append' A1      _      (T1 t)    u         = t :| u
              append' (A1S s) _      (t :| ts) u         = t :|
                                                           append' s SOne ts u
              append' (An s)  (SS n) (T1 t)    (T1 u)    = T1 $
                                                           append' s n t u
              append' (An s)  (SS n) (t :| ts) (u :| us) =
                  append' s n t u :| append' (An s) (SS n) ts us
    split = split' appendSing
        where split' ∷ Append n is js ks
                     → SPI n → Tensor ks e → (Tensor is e, Tensor js e)
              split' A1      _      (t :| u ) = (T1 t, u)
              split' (A1S s) _      (t :| ts) = let (u, v) = split' s SOne ts
                                                in (t :| u, v)
              split' (An s)  (SS n) (T1 t)    = let (u, v) = split' s n t
                                                in (T1 u, T1 v)
              split' (An s)  (SS n) (t :| ts) =
                  let (u,  v ) = split' s      n      t
                      (us, vs) = split' (An s) (SS n) ts
                  in (u :| us, v :| vs)

-----------------------------------  IsList  -----------------------------------

-- | The list representation of @'Tensor'@ uses [row-major
-- order](http://en.wikipedia.org/wiki/Row-major_order).
instance SingI is ⇒ IsList (Tensor is e) where
    type Item (Tensor is e) = e
    fromList = f sing
        where f ∷ Shape js → [e] -> Tensor js e
              f SNil              [e] = T0 e
              f SNil              _   = throw WrongListLength
              f (SCons SOne is)   l   = T1 $ f is l
              f (SCons (SS i) is) l   =
                  let a = fromPI (SS i)
                      b = product $ fromShape is
                  in if length l ≡ a ⋅ b
                     then uncurry (:|) $ (f is *** f (SCons i is)) $ splitAt b l
                     else throw WrongListLength
    toList = toList'
        where toList' ∷ Tensor js e → [e]
              toList' (T0 e) = [e]
              toList' (T1 t) = toList' t
              toList' (t :| ts) = toList' t ++ toList' ts

-----------------------------------  NFData  -----------------------------------

-- | Recursively evaluate the @'Tensor'@.
instance NFData e ⇒ NFData (Tensor is e) where
    rnf (T0 e)    = rnf e
    rnf (T1 t)    = rnf t
    rnf (t :| ts) = rnf t `seq` rnf ts

-----------------------------------  Random  -----------------------------------

-- | Random @'Tensor'@ with independent and identically distributed components.
instance (Random e, SingI is) ⇒ Random (Tensor is e) where
    randomR = r sing
        where r ∷ RandomGen g ⇒
                Shape js → (Tensor js e, Tensor js e) → g → (Tensor js e, g)
              r SNil              (T0 a, T0 b)       = first T0 ∘ randomR (a, b)
              r (SCons SOne   is) (T1 t, T1 u)       = first T1 ∘ r is (t, u)
              r (SCons (SS i) is) (t :| ts, u :| us) =
                  (\(x, (y, z)) → (x :| y, z)) ∘
                  second (r (SCons i is) (ts, us)) ∘ r is (t, u)
    random = r sing
        where r ∷ RandomGen g ⇒ Shape js → g → (Tensor js e, g)
              r SNil              = first T0 ∘ random
              r (SCons SOne   is) = first T1 ∘ r is
              r (SCons (SS i) is) =
                  (\(x, (y, z)) → (x :| y, z)) ∘ second (r (SCons i is)) ∘ r is

----------------------------------  Sliceable ----------------------------------

-- | An @'IsSlicer'@ type custom fitted for the @'PI'@ kind and the @'Tensor'@
-- type.
data Slicer ∷ [PI] → [Maybe PI] → [PI] → * where
    NilS       ∷ Slicer '[] '[] '[]
    AllCons    ∷ Slicer is js ks
               → Slicer (i ': is) ('Nothing ': js) (i ': ks)
    OneConsSl  ∷ Slicer is js ks
               → Slicer (i ': is) ('Just i ': js) ks
    HeadSuccSl ∷ Slicer (i ': is) ('Just i ': js) ks
               → Slicer ('S i ': is) ('Just ('S i) ': js) ks

-- | Trivial instance.
instance IsSlicer Slicer where
    nilS = NilS
    allCons = AllCons
    OneCons _  & sl = OneConsSl sl
    HeadSucc i & sl = HeadSuccSl (i & sl)
    toSlicer = toSlicer' slicerShape
        where toSlicer' ∷ SlicerShape is js ks
                        → Slicer is js ks → S.Slicer is js ks
              toSlicer' _ NilS            = S.NilS
              toSlicer' _ (AllCons    sl) = S.AllCons $ toSlicer' undefined sl
              toSlicer' _ (OneConsSl  sl) =
                  OneCons Nil :& toSlicer' undefined sl
              toSlicer' _ (HeadSuccSl sl) = bumpSl $ toSlicer' undefined sl
              bumpSl ∷ S.Slicer (i ': is) ('Just i ': js) ks
                     → S.Slicer ('S i ': is) ('Just ('S i) ': js) ks
              bumpSl (i :& s) = HeadSucc i :& s

-- | Recursively defined slicing.
instance Sliceable Tensor where
    type Sl Tensor = Slicer
    slice (T0 e) NilS = T0 e
    slice (T1 t) (AllCons sl) = T1 $ slice t sl
    slice (t :| ts) (AllCons sl) = slice t sl :| slice ts (AllCons sl)
    slice (T1 t)    (OneConsSl sl) = slice t sl
    slice (t :| _ ) (OneConsSl sl) = slice t sl
    slice (_ :| ts) (HeadSuccSl sl) = slice ts sl

--------------------------------------------------------------------------------
