{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  © 2012-2013 Nicola Squartini
-- License     :  GPL-3
--
-- Maintainer  :  Nicola Squartini <tensor5@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
--------------------------------------------------------------------------------

module Data.TypeAlgebra where


-- | Sum of types.
class Sum a b where
    type a :+: b
    -- | The sum of an element of @a@ and an element of @b@ is an
    -- element in the type @a ':+:' b@.
    (<+>) :: a -> b -> a :+: b


-- | Product of types.
class Prod a b where
    type a :*: b
    -- | The product of an element of @a@ and an element of @b@ is an
    -- element in the type @a ':*:' b@.
    (<*>) :: a -> b -> a :*: b

