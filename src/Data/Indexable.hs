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
-- @'Indexable'@ and @'MultiIndexable'@ classes.
--
--------------------------------------------------------------------------------

module Data.Indexable where

import           Data.Functor.Identity
import           Data.Singletons
import           Prelude.Unicode


-- | Class of maps from an index type to an element type.
class Indexable (v ∷ χ → * → *) where
    type Index v ∷ χ → *
    (!) ∷ v i e → Index v i → e
    -- | Generate an indexable type from an (index → element) map.
    generate ∷ SingI i ⇒ (Index v i → e) → v i e
    generate = runIdentity ∘ generateA ∘ ((∘) Identity)
    -- | Same as @'generate'@ but works inside an @'Applicative'@ functor.
    generateA ∷ (Applicative f, SingI i) ⇒ (Index v i → f e) → f (v i e)
    -- | Same as @'generate'@ but works inside a @'Monad'@.
    generateM ∷ (Monad m, SingI i) ⇒ (Index v i → m e) → m (v i e)
    generateM = generateA
    map ∷ (e → f) → v i e → v i f
    ap ∷ v i (e → f) → v i e → v i f


-- | @'castIndexable' f g v = 'generate' (g ∘ ('!') v ∘ f)@
castIndexable ∷ ( Indexable v1
             , Indexable v2
             , SingI js
             ) ⇒ (Index v2 js → Index v1 is)
               → (e → f)
               → v1 is e
               → v2 js f
castIndexable f g v = generate (g ∘ (!) v ∘ f)


-- | Same as @'Indexable'@ but the @'Index'@ type is an heterogeneous list.
class Indexable t ⇒ MultiIndexable (t ∷ [χ] → * → *) where
    t0 ∷ e → t '[] e
    unT0 ∷ t '[] e → e
--    unT0 t = t ! nil
    concat ∷ t (i ': is) (t js e) → t is (t (i ': js) e)
    unConcat ∷ t is (t (i ': js) e) → t (i ': is) (t js e)
    at ∷ t (i ': is) e → Index t is → t '[i] e
    ta ∷ Index t '[i] → t (i ': is) e → t is e
    -- | @'concat'@ until the outside tensor has rank 0 and then @'unT0'@.
    --
    -- @'rev' ≡ 'unT0' ∘ 'concat' ∘ … ∘ 'concat'@
    rev ∷ ReverseI is js ks ⇒ t is (t js e) → t ks e
    -- | @'unConcat'@ until the inside tensors has rank 0 and then @'unT0'@ on
    -- the inside tensors.
    --
    -- @'unRev' ≡ 'map' 'unT0' ∘ 'unConcat' ∘ … ∘ 'unConcat'@
    unRev ∷ ReverseI is js ks ⇒ t js (t is e) → t ks e


-- | Expresses the possible ways to reverse a list concatenating to a second
-- list.
data Reverse ∷ [χ] → [χ] → [χ] → * where
    R0 ∷ Reverse '[] js js
    R1 ∷ Reverse is (i ': js) ks → Reverse (i ': is) js ks

-- | Class for implicit @'Reverse'@ parameter.
class ReverseI is js ks | is js → ks where
    reverseSing ∷ Reverse is js ks

-- | Do nothing (@'reverseSing' = 'R0'@).
instance ReverseI '[] js js where
    reverseSing = R0

-- | Append @i@ to @is@ and iterate (@'reverseSing' = 'R1' 'reverseSing'@).
instance ReverseI is (i ': js) ks ⇒ ReverseI (i ': is) js ks where
    reverseSing = R1 reverseSing


-- | Rearrange the indeces and the corresponding elements in reverse order.
transpose ∷ (MultiIndexable t, ReverseI is '[] js) ⇒ t is e → t js e
transpose = unRev ∘ t0
