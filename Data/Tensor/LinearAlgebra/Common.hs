{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.Tensor.LinearAlgebra.Common where

import           Data.Cardinal
import           Data.Ordinal


class Num e => VectorSpace e v | v -> e where
    zero ∷ v
    (*.) ∷ e -> v -> v
    (.+.) ∷ v -> v -> v
--    dimension ∷ v -> Integer


class (Cardinal n) => Product e n t1 t2 | t1 -> e, t2 -> e, t1 t2 -> e where
    type ProdSpace n t1 t2
    prod ∷ n -> t1 -> t2 -> ProdSpace n t1 t2


class MatrixProduct e t1 t2 | t1 -> e, t2 -> e where
    type MatrixProductSpace t1 t2
    (.*.) ∷ t1 -> t2 -> MatrixProductSpace t1 t2


class TensorProduct e t1 t2 | t1 -> e, t2 -> e where
    type t1 :⊗: t2
    (⊗) ∷ t1 -> t2 -> t1 :⊗: t2


class DotProduct t where
    dot ∷ (Num e) => t e -> t e -> e


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
