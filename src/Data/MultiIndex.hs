{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE Safe              #-}
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

module Data.MultiIndex
    ( PI(..)
    , IsMultiIndex(..)
    , Shape(..)
    , flatten
    , fromShape
    , ShapeI(..)
    , MultiIndex(..)
    ) where

import           Prelude.Unicode

import           Data.Sliceable  (Key, KeyShape)


-- | Kind of positive (non-zero) integers.
data PI = One
        | S PI
          deriving Eq

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
    headSucc ∷ m (i ': is) → m (S i ': is)
    fromMultiIndex ∷ MultiIndex is → m is
    fromMultiIndex Nil           = nil
    fromMultiIndex (OneCons is)  = oneCons $ fromMultiIndex is
    fromMultiIndex (HeadSucc is) = headSucc $ fromMultiIndex is
    toMultiIndex ∷ ShapeI is ⇒ m is → MultiIndex is
    fromList ∷ (ShapeI is, Num a, Ord a) ⇒ [a] → Maybe (m is)
    fromList = toM shape
        where toM ∷ (IsMultiIndex m, Num a, Ord a) ⇒
                  Shape is → [a] → Maybe (m is)
              toM Point         []       = Just nil
              toM Point         _        = Nothing
              toM (AddDim sh)   (1 : xs) = case toM sh xs of
                                             Nothing → Nothing
                                             Just is → Just (oneCons is)
              toM (AddDim _)    _        = Nothing
              toM (AddLayer sh) (x:xs)
                  | x > 1     = case toM sh ((x - 1) : xs) of
                                  Nothing → Nothing
                                  Just is → Just (headSucc is)
                  | x ≡ 1     = case toM (flatten sh) xs of
                                  Nothing → Nothing
                                  Just is → Just (oneCons is)
                  | otherwise = Nothing
              toM (AddLayer _)  _        = Nothing
    toList ∷ Num a ⇒ m is → [a]

-- | Singleton type for the kind @['PI']@.
data Shape (is ∷ [PI]) where
    Point    ∷ Shape '[]
    AddDim   ∷ Shape is → Shape (One ': is)
    AddLayer ∷ Shape (i ': is) → Shape (S i ': is)

-- | Reduce the dimension of @'Shape'@ by eliminating the first index.
flatten ∷ Shape (i ': is) → Shape is
flatten (AddDim is)   = is
flatten (AddLayer is) = flatten is

-- | Convert a @'Shape'@ into a @'Num'@eric list.
fromShape ∷ Num a ⇒ Shape is → [a]
fromShape Point         = []
fromShape (AddDim is)   = 1 : fromShape is
fromShape (AddLayer is) = let x : xs = fromShape is
                          in (x + 1) : xs

-- | Class for implicit @'Shape'@ parameter.
class ShapeI (is ∷ [PI]) where
    shape ∷ Shape is

instance ShapeI '[] where
    shape = Point

instance ShapeI is ⇒ ShapeI (One ': is) where
    shape = AddDim shape

instance ShapeI (i ': is) ⇒ ShapeI (S i ': is) where
    shape = AddLayer shape

-- | A multidimensional index with fixed size and fixed (non-zero) dimensions.
data MultiIndex ∷ [PI] → * where
    Nil      ∷ MultiIndex '[]
    OneCons  ∷ MultiIndex is → MultiIndex (i ': is)
    HeadSucc ∷ MultiIndex (i ': is) → MultiIndex (S i ': is)

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

type instance KeyShape (a ∷ PI) = Shape '[a]
