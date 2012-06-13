{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Tensor where


-- | A @'Tensor'@ is a map from an @'Index'@ type (which should be a
-- @'MultiIndex'@) to an @'Elem'@ent type.
class Tensor t where
    type Index t
    type Elem t
    -- | @'dims'@ returns the dimensions of the @'Tensor'@. In any
    -- instance of @'Tensor'@ @'dims'@ should be independent of its
    -- argument and work on @'undefined'@.
    dims :: t -> Index t
    -- | Returns the @'Elem'@ent of @t@ corresponding to @'Index' t@.
    (!) :: t -> Index t -> Elem t
    -- | Generates a @'Tensor'@ according to the given function.
    generate :: (Index t -> Elem t) -> t
    -- | Generates a @'Tensor'@ consisting of the same @'Elem'@ent
    -- repeated.
    replicate :: Elem t -> t
    replicate e = generate (\_ -> e)


class FromList t where
    fromList :: [e] -> t e


class DirectSum n t1 t2 where
    type SumSpace n t1 t2
    directSum :: n -> t1 -> t2 -> SumSpace n t1 t2


class Transpose t where
    type TransposeSpace t
    transpose :: t -> TransposeSpace t


class Zip t where
    zipWith :: (a -> b -> c) -> t a -> t b -> t c

