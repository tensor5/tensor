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

{-Alas, GHC 7.0 still cannot handle equality superclasses
rowSwitch ∷ (Ordinal i, Ordinal j ,MultiIndexable t, (Index t) ~ (i :|: (j :|: Nil)))
              => i -> i -> t -> t
rowSwitch _ = undefined

rowMult ∷ (Ordinal i, Ordinal j ,MultiIndexable t, (Index t) ~ (i :|: (j :|: Nil)), (Elem t) ~ e, Num e)
            => i -> e -> t -> t
rowMult _ = undefined

rowAdd ∷ (Ordinal i, Ordinal j ,MultiIndexable t, (Index t) ~ (i :|: (j :|: Nil)), (Elem t) ~ e, Num e)
           => i -> e -> i -> t -> t
rowAdd _ = undefined

colSwitch ∷ (Ordinal i, Ordinal j ,MultiIndexable t, (Index t) ~ (i :|: (j :|: Nil)))
              => j -> j -> t -> t
colSwitch _ = undefined

colMult ∷ (Ordinal i, Ordinal j ,MultiIndexable t, (Index t) ~ (i :|: (j :|: Nil)), (Elem t) ~ e, Num e)
        => j -> e -> t -> t
colMult _ = undefined

colAdd ∷ (Ordinal i, Ordinal j ,MultiIndexable t, (Index t) ~ (i :|: (j :|: Nil)), (Elem t) ~ e, Num e)
           => j -> e -> j -> t -> t
colAdd _ = undefined
-}

class DirectSummable n t1 t2 where
    type DirectSum n t1 t2
    cat ∷ n → t1 → t2 → DirectSum n t1 t2


class Transpose t where
    type TransposeSpace t
    transpose ∷ t -> TransposeSpace t


class Zip t where
    zipWith :: (a -> b -> c) -> t a -> t b -> t c

