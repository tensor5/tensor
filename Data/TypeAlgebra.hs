{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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

