{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Tensor where

import           Data.MultiIndex
import           Data.Ordinal


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
    -- | Computes the coefficient of the polynomial p(z)=det(A+zI)
    -- using the method of /closed ordered walks/ (/clow/) illustrated
    -- in the paper of G. Rote
    -- <http://page.mi.fu-berlin.de/rote/Papers/pdf/Division-free+algorithms.pdf>.
    -- The number of operations for the whole process is O(n^4), where
    -- n is the number of rows of the matrix. The first coefficient is
    -- the known term and equals the determinant, while the last one
    -- is the coefficient of z^(n-1) and equals the trace. The
    -- coefficient of z^n equals 1 and is not included in the
    -- resulting list. The k-th coefficient is the sum of all
    -- principal minors of order n-k+1.
    charPoly :: t -> [e]
    det :: t -> e
