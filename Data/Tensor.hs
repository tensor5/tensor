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
    -- | Returns the @'Elem'@ent of @t@ corresponding to @'Index' t@.
    (!) :: t -> Index t -> Elem t
    -- | Generates a @'Tensor'@ according to the given function.
    generate :: (Index t -> Elem t) -> t


-- | Generates a @'Tensor'@ consisting of the same @'Elem'@ent
-- repeated.
replicate :: Tensor t => Elem t -> t
replicate e = generate (\_ -> e)


-- | @'elemMap' f t@ applies @f@ to every @'Elem'@ent of @t@.
elemMap :: (Tensor t1, Tensor t2, Index t1 ~ Index t2) => (Elem t1 -> Elem t2) -> t1 -> t2
elemMap f t = generate (\i -> f (t ! i))


-- | In @'indexMap' f t@, the @'Elem'@ent corresponding to the
-- @'Index'@ @i@ is the @'Elem'@ent that @t@ assignes to the @'Index'@
-- @f i@.
indexMap :: (Tensor t1, Tensor t2, Elem t1 ~ Elem t2) =>
            (Index t1 -> Index t2) -> t2 -> t1
indexMap f t = generate (\i -> t ! f i)


class FromList t where
    fromList :: [e] -> t e


class DirectSum n t1 t2 where
    type SumSpace n t1 t2
    directSum :: n -> t1 -> t2 -> SumSpace n t1 t2
    split :: n -> SumSpace n t1 t2 -> (t1,t2)


class Transpose t where
    type TransposeSpace t
    transpose :: t -> TransposeSpace t


class Zip t where
    zipWith :: (a -> b -> c) -> t a -> t b -> t c


-- | Slices the @'Tensor'@ @t@ by dropping @i@ at the beginning of its @'Index'@
-- and @j@ at the end. The result has type @'Slice' i j t@.
class Sliceable i j t where
    type Slice i j t
    -- | Extracts the @'Slice'@ of @t@ for the given initial and final
    -- indices @i@ and @j@.
    slice :: i -> j -> t -> Slice i j t

