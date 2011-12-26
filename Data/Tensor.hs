{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Tensor where

import           Data.MultiIndex
import           Data.Ordinal
import qualified Data.Vector as V
import           Data.Vector (Vector)


class FromVector e t | t -> e where
    fromVector ∷ Vector e -> t


class FromList e t | t -> e where
    fromList ∷ [e] -> t


-- | In any instance of @'MultiIndexable'@ @'dims'@ should be
-- independent of its argument and work on @'undefined'@.
class MultiIndex i => MultiIndexable i e t | t -> e, t -> i where
    (!) ∷ t -> i -> e
    dims ∷ t -> i



class DirectSummable n t1 t2 e | t1 → e, t2 → e where
    type DirectSum n t1 t2
    cat ∷ n → t1 → t2 → DirectSum n t1 t2

instance (HNat2Integral n, MultiIndex i, MultiIndex j, MultiIndexConcat n i j)
    ⇒ DirectSummable n (Tensor i e) (Tensor j e) e where
        type DirectSum n (Tensor i e) (Tensor j e) = (Tensor (Concat n i j) e)
        cat n (Tensor d x) (Tensor d' y) = Tensor ((take i d) ++ e'') (V.generate l g)
            where l = foldl1 (*) ((take i d) ++ e'')
                  e = drop i d
                  e' = drop i d'
                  e'' = ((d !! i) + (d' !! i)) : (drop (i+1) d)
                  m = foldl1 (*) e
                  m' = foldl1 (*) e'
                  m'' = foldl1 (*) e''
                  g k = if rem k m'' < m
                        then x V.! ((quot k m'')*m + (rem k m''))
                        else y V.! ((quot k m'')*m' + (rem k m'') - m)
                  i = hNat2Integral n


class Num e => VectorSpace e v | v -> e where
    zero ∷ v
    (*.) ∷ e -> v -> v
    (.+.) ∷ v -> v -> v
--    dimension ∷ v -> Integer

class (MultiIndex i) => Product e i t1 t2 t3 | t1 -> e, t2 -> e, i t1 t2 -> e where
    prod ∷ i -> t1 -> t2 -> t3


class MatrixProduct e t1 t2 t3 | t1 t2 -> t3, t1 -> t2, t2 -> e where
    (.*.) ∷ t1 -> t2 -> t3


class TensorProduct e t1 t2 t3 | t1 -> e, t2 -> e, t1 t2 -> t3 where
    (⊗) ∷ t1 -> t2 -> t3


class Transpose t1 t2 | t1 -> t2 where
    transpose ∷ t1 -> t2


class DotProduct e a b c | a b -> c, a -> e, b -> e where
    dot ∷ a -> b -> c


class (Num e, Ordinal i, Ordinal j) => RMatrix e i j t
                                     | t -> e, t -> i, t -> j where
    rowSwitch ∷ i -> i -> t -> t
    rowMult ∷ i -> e -> t -> t
    rowAdd ∷ i -> e -> i -> t -> t
    colSwitch ∷ j -> j -> t -> t
    colMult ∷ j -> e -> t -> t
    colAdd ∷ j -> e -> j -> t -> t


class (Fractional e, Ordinal i, Ordinal j) => EchelonForm e i j t
                                            | t -> e, t -> i, t -> j where
    rowEchelonForm ∷ t -> t


class (Fractional e, Ordinal i, Ordinal j) => LinearSystem e i j t1 t2
                              | t1 -> e, t1 -> i, t1 -> j, e -> t1, e -> i where
    solveLinSystem ∷ t1 -> t2 -> (t1,t2)


class (Fractional e, Ordinal i) => SquareMatrix e i t | t -> e, t -> i where
    unit ∷ t
    inverse ∷ t -> Maybe t
    tr ∷ t -> e