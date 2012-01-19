{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Tensor where

import           Data.TypeList.MultiIndex


class FromList t where
    fromList ∷ [e] -> t e


-- | In any instance of @'MultiIndexable'@ @'dims'@ should be
-- independent of its argument and work on @'undefined'@.
class MultiIndex i => MultiIndexable i e t | t -> e, t -> i where
    (!) ∷ t -> i -> e
    dims ∷ t -> i



class DirectSummable n t1 t2 e | t1 → e, t2 → e where
    type DirectSum n t1 t2
    cat ∷ n → t1 → t2 → DirectSum n t1 t2


class Transpose t1 t2 | t1 -> t2 where
    transpose ∷ t1 -> t2


