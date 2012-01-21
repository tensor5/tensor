{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Tensor where


class FromList t where
    fromList ∷ [e] -> t e


-- | In any instance of @'Tensor'@ @'dims'@ should be
-- independent of its argument and work on @'undefined'@.
class Tensor t where
    type Index t
    type Elem t
    dims ∷ t -> Index t
    (!) ∷ t -> Index t -> Elem t
    generate :: (Index t -> Elem t) -> t
    replicate :: Elem t -> t
    replicate e = generate (\_ -> e)


class DirectSum n t1 t2 where
    type SumSpace n t1 t2
    directSum ∷ n → t1 → t2 → SumSpace n t1 t2


class Transpose t where
    type TransposeSpace t
    transpose ∷ t -> TransposeSpace t


class Zip t where
    zipWith :: (a -> b -> c) -> t a -> t b -> t c

