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


-- | The type @a@ is @'Next'@ to @b@ if @a@ can be canonically
-- embedded into @b@.
{-
class Next s where
    embed :: a -> s a
    succ :: a -> s a
    pred :: s a -> a
-}

class SubSet a b where
    embed :: a -> b

instance SubSet a a where
    embed = id

--instance (SubSet a b, SubSet b c) => SubSet a c where
--    embed = embed . embed

class LEq a b

instance LEq a a

instance (LEq a b, LEq b c) => LEq a c

