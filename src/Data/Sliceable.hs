{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Trustworthy            #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  © 2015 Nicola Squartini
-- License     :  BSD3
--
-- Maintainer  :  Nicola Squartini <tensor5@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- @'Sliceable'@ types are data types indexed by a list of types (keys), for
-- which is possible to specify the values of any subset of keys. The selection
-- of keys is represented by the @'Slicer'@ type, or any other type in the
-- @'IsSlicer'@ class.
--
--------------------------------------------------------------------------------

module Data.Sliceable where

import           Data.Singletons


-- | A polykinded type representing a key.
type family Key (a ∷ χ) ∷ *

type instance Key (a ∷ *) = a

-- | Type for describing an exact match for a list of keys. It has three
-- parameters:
--
-- 1. the kind of the list of keys
--
-- 2. the list of matches: @'Nothing'@ means that no specific value is requested
-- on that key, while @'Just' k@ means that the corresponding key in the first
-- list must have value @k@
--
-- 3. the resulting list of keys where the keys in the first list for which a
-- match (@'Just'@) is required in the second list have been eliminated.
data Slicer ∷ [χ] → [Maybe χ] → [χ] → * where
    NilS    ∷ Slicer '[] '[] '[]
    AllCons ∷ Slicer is js ks → Slicer (i ': is) ('Nothing ': js) (i ': ks)
    (:&)    ∷ Key i → Slicer is js ks → Slicer (i ': is) ('Just i ': js) ks

-- | Singleton type, represents the shape of a @'Slicer'@ ignoring the specific
-- value selections.
data SlicerShape ∷ [χ] → [Maybe χ] → [χ] → * where
    NilSh     ∷ SlicerShape '[] '[] '[]
    AllConsSh ∷ SlicerShape is js ks
              → SlicerShape (i ': is) ('Nothing ': js) (i ': ks)
    (:$)      ∷ Sing i
              → SlicerShape is js ks
              → SlicerShape (i ': is) ('Just i ': js) ks

-- | Class for implicit @'SlicerShape'@ parameter.
class SlicerShapeI (is ∷ [χ]) (js ∷ [Maybe χ]) (ks ∷ [χ]) | is js → ks where
    slicerShape ∷ SlicerShape is js ks

-- | No selection can be done on an empty list (@'slicerShape' = 'NilSh'@).
instance SlicerShapeI '[] '[] '[] where
    slicerShape = NilSh

-- | No selection on the first component (@'slicerShape' = 'AllConsSh'
-- 'slicerShape'@).
instance SlicerShapeI is js ks ⇒
    SlicerShapeI (i ': is) ('Nothing ': js) (i ': ks) where
    slicerShape = AllConsSh slicerShape

-- | Select something on the first component (@'slicerShape' = 'sing' ':$'
-- 'slicerShape'@).
instance ( SingI i
         , SlicerShapeI is js ks
         ) ⇒ SlicerShapeI (i ': is) ('Just i ': js) ks where
    slicerShape = sing :$ slicerShape

-- | Class of types isomorphic to @'Slicer'@. Instances should satisfy the
-- following properties:
--
-- @'fromSlicer' 'NilS' ≡ 'nilS'@
--
-- @'fromSlicer' '.' 'AllCons' ≡ 'allCons' '.' 'fromSlicer'@
--
-- @'fromSlicer' '.' (':&') k ≡ ('&') k '.' 'fromSlicer'@
--
-- @'toSlicer' '.' 'fromSlicer' ≡ 'id'@
--
-- @'fromSlicer' '.' 'toSlicer' ≡ 'id'@
class IsSlicer (s ∷ [χ] → [Maybe χ] → [χ] → *) where
    nilS ∷ s '[] '[] '[]
    allCons ∷ s is js ks → s (i ': is) ('Nothing ': js) (i ': ks)
    (&) ∷ Key i → s is js ks → s (i ': is) ('Just i ': js) ks
    fromSlicer ∷ Slicer is js ks → s is js ks
    fromSlicer NilS = nilS
    fromSlicer (AllCons sl) = allCons $ fromSlicer sl
    fromSlicer (k :& sl) = k & fromSlicer sl
    toSlicer ∷ SlicerShapeI is js ks ⇒ s is js ks → Slicer is js ks

-- | Trivial instance (@'fromSlicer' = 'id'@, @'toSlicer' = 'id'@).
instance IsSlicer Slicer where
    nilS = NilS
    allCons = AllCons
    (&) = (:&)
    fromSlicer = id
    toSlicer = id

-- | Class of types that allow taking a @'slice'@, i.e. specifying the values of
-- a subset of keys. The selection of key values is expressed by the @'Sl'@
-- type, which is instance of @'IsSlicer'@ and therefore isomorphic to
-- @'Slicer'@, but being an associated type allows custom representation.
class IsSlicer (Sl t) ⇒ Sliceable (t ∷ [χ] → * → *) where
    type Sl t ∷ [χ] → [Maybe χ] → [χ] → *
    slice ∷ t i e → Sl t i j k → t k e
