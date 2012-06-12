{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Tensor.LinearAlgebra where

import Data.Cardinal
import Data.Tensor
import Data.TypeList.MultiIndex

class VectorSpace v where
    zero :: Num e => v e
    (*.) :: Num e => e -> v e -> v e
    (.+.) :: Num e => v e -> v e -> v e


class (Cardinal n) => Product n t1 t2 where
    type ProdSpace n t1 t2
    prod :: n -> t1 -> t2 -> ProdSpace n t1 t2


class MatrixProduct t1 t2 where
    type MatrixProductSpace t1 t2
    (.*.) :: t1 -> t2 -> MatrixProductSpace t1 t2


class TensorProduct t1 t2 where
    type t1 :⊗: t2
    (⊗) :: t1 -> t2 -> t1 :⊗: t2


class DotProduct t where
    dot :: (Num e) => t e -> t e -> e


class (Tensor t, (Index t) ~ (i :|: (j :|: Nil))) =>
    Matrix i j t where
        rowSwitch :: i -> i -> t -> t
        rowMult :: (Num e, (Elem t) ~ e) => i -> (Elem t) -> t -> t
        rowAdd :: (Num e, (Elem t) ~ e) => i -> (Elem t) -> i -> t -> t
        colSwitch :: j -> j -> t -> t
        colMult :: (Num e, (Elem t) ~ e) => j -> (Elem t) -> t -> t
        colAdd :: (Num e, (Elem t) ~ e) => j -> (Elem t) -> j -> t -> t
        rowEchelonForm :: (Eq e, Fractional e, (Elem t) ~ e) => t -> t


class LinearSystem t1 t2 where
    type SolSpace t1 t2
    triangularSolve :: t1 -> t2 -> (t1,t2)
    parametricSolve :: t1 -> t2 -> Maybe (SolSpace t1 t2,[SolSpace t1 t2])


class SquareMatrix t where
    unit :: Num e => t e
    inverse :: (Eq e, Fractional e) => t e -> Maybe (t e)
    tr :: Num e => t e -> e
    tr = last . charPoly
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
    charPoly :: Num e => t e -> [e]
    det :: Num e => t e -> e
    det = head . charPoly
