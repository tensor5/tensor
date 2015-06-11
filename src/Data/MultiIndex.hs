{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  © 2014-2015 Nicola Squartini
-- License     :  BSD3
--
-- Maintainer  :  Nicola Squartini <tensor5@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A multi-index is a list of positive integers (indeces). When used to identify
-- elements in multi dimensional arrays, all sharing the same fixed dimensions,
-- the list of indeces should not exceed the predefined dimensions, which are
-- collectively called the /shape/ of the multi-index. This module defines the
-- @'MultiIndex'@ type for describing fixed shape multi-indeces, i.e. list of
-- integers ranging from 1 to a preset maximum which is encoded in the parameter
-- of @'MultiIndex'@.
--
--------------------------------------------------------------------------------

module Data.MultiIndex where

import           Data.Singletons
import           Data.Singletons.Prelude.List
import           Prelude.Unicode

import           Data.Sliceable               (Key)


-- | Kind of positive (non-zero) integers.
data PI = One
        | S PI
          deriving Eq

data instance Sing (n ∷ PI) where
    SOne ∷ Sing 'One
    SS   ∷ Sing n → Sing ('S n)

-- | Kind-restristed synonym for @'Sing'@, expressing a positive integer at type
-- level: @type 'SPI' (n ∷ 'PI') = 'Sing' n@.
type SPI (n ∷ PI) = Sing n

-- | Kind-restricted synonym for @'Sing'@, expressing the shape of a multiindex
-- or a multidimensional array: @type 'Shape' (is ∷ ['PI']) = 'Sing' is@.
type Shape (is ∷ [PI]) = Sing is

-- | Convert a singleton positive integer into a @'Num'@.
fromPI ∷ Num a ⇒ SPI n → a
fromPI SOne   = 1
fromPI (SS n) = 1 + fromPI n

instance SingI 'One where
    sing = SOne

instance SingI n ⇒ SingI ('S n) where
    sing = SS sing

-- | Class of types isomorphic to @'MultiIndex'@. Instances should satisfy the
-- following properties:
--
-- @'fromMultiIndex' 'Nil' ≡ 'nil' @
--
-- @'fromMultiIndex' '.' 'OneCons' ≡ 'oneCons' '.' 'fromMultiIndex'@
--
-- @'toMultiIndex' '.' 'fromMultiIndex' ≡ 'id'@
--
-- @'fromMultiIndex' '.' 'toMultiIndex' ≡ 'id'@
class IsMultiIndex (m ∷ [PI] → *) where
    nil ∷ m '[]
    oneCons ∷ m is → m (i ': is)
    headSucc ∷ m (i ': is) → m ('S i ': is)
    fromMultiIndex ∷ MultiIndex is → m is
    fromMultiIndex Nil           = nil
    fromMultiIndex (OneCons is)  = oneCons $ fromMultiIndex is
    fromMultiIndex (HeadSucc is) = headSucc $ fromMultiIndex is
    toMultiIndex ∷ SingI is ⇒ m is → MultiIndex is
    fromList ∷ (SingI is, Num a, Ord a) ⇒ [a] → Maybe (m is)
    fromList = toM sing
        where toM ∷ (IsMultiIndex m, Num a, Ord a) ⇒
                  Shape is → [a] → Maybe (m is)
              toM SNil              []       = Just nil
              toM SNil              _        = Nothing
              toM (SCons SOne   sh) (1 : xs) = case toM sh xs of
                                                 Nothing → Nothing
                                                 Just is → Just (oneCons is)
              toM (SCons SOne   _ ) _        = Nothing
              toM (SCons (SS i) sh) (x:xs)
                  | x > 1     = case toM (SCons i sh) ((x - 1) : xs) of
                                  Nothing → Nothing
                                  Just is → Just (headSucc is)
                  | x ≡ 1     = case toM sh xs of
                                  Nothing → Nothing
                                  Just is → Just (oneCons is)
                  | otherwise = Nothing
              toM (SCons (SS _) _ ) _        = Nothing
    toList ∷ Num a ⇒ m is → [a]


-- | Convert a @'Shape'@ into a @'Num'@eric list.
fromShape ∷ Num a ⇒ Shape is → [a]
fromShape SNil         = []
fromShape (SCons i is) = fromPI i : fromShape is

-- | A multidimensional index with fixed size and fixed (non-zero) dimensions.
data MultiIndex ∷ [PI] → * where
    Nil      ∷ MultiIndex '[]
    OneCons  ∷ MultiIndex is → MultiIndex (i ': is)
    HeadSucc ∷ MultiIndex (i ': is) → MultiIndex ('S i ': is)

instance Eq (MultiIndex is) where
    Nil         == Nil         = True
    Nil         == _           = False
    OneCons  is == OneCons  js = is ≡ js
    OneCons  _  == _           = False
    HeadSucc is == HeadSucc js = is ≡ js
    HeadSucc _  == _           = False

instance Ord (MultiIndex '[i]) where
    compare (OneCons  _) (OneCons  _) = EQ
    compare (OneCons  _) (HeadSucc _) = LT
    compare (HeadSucc _) (OneCons  _) = GT
    compare (HeadSucc i) (HeadSucc j) = compare i j

instance Show (MultiIndex is) where
    showsPrec n is = showsPrec n (toList is ∷ [Integer])

instance IsMultiIndex MultiIndex where
    nil = Nil
    oneCons = OneCons
    headSucc = HeadSucc
    fromMultiIndex = id
    toMultiIndex = id
    toList Nil           = []
    toList (OneCons is)  = 1 : toList is
    toList (HeadSucc is) = let x : xs = toList is
                           in (x + 1) : xs


type instance Key (a ∷ PI) = MultiIndex '[a]
