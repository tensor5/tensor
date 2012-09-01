{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Tensor.LinearAlgebra where

import Data.Cardinal
import Data.Tensor
import Data.TypeList.MultiIndex hiding (head)

class VectorSpace v where
    zero :: Num e => v e
    (*.) :: Num e => e -> v e -> v e
    (.+.) :: Num e => v e -> v e -> v e


-- | A general form of product between two tensors, in which the last
-- @n@ dimensions of @t1@ are contracted with the first @n@ dimensions
-- of @t2@. The resulting tensor belongs to the space @'ProdSpace' n
-- t1 t2@. The operators @'.*.'@ and @⊗@ below are particular cases
-- where @n@ is equal to 1 and 0 respectively.
class (Cardinal n) => Product n t1 t2 where
    type ProdSpace n t1 t2
    prod :: n -> t1 -> t2 -> ProdSpace n t1 t2


type MatrixProductSpace t1 t2 = ProdSpace (Succ Zero) t1 t2

-- | It is the product of the last dimension of @t1@ with the first
-- dimension of @t2@. In the case where @t1@ and @t2@ are matrices this
-- coincide with the ordinary matrix product.
(.*.) :: (Product (Succ Zero) t1 t2) => t1 -> t2 -> MatrixProductSpace t1 t2
x .*. y = prod (undefined :: C1) x y


type t1 :⊗: t2 = ProdSpace C0 t1 t2

-- | Tensor product of @t1@ and @t2@.
(⊗) :: (Product C0 t1 t2) => t1 -> t2 -> t1 :⊗: t2
(⊗) = prod (undefined :: C0)


class DotProduct t where
    dot :: (Num e) => t e -> t e -> e


-- | A matrix with @i@ rows and @j@ columns.
class (Tensor t, (Index t) ~ (i :|: (j :|: Nil))) =>
    Matrix i j t where
        -- | Switch two rows.
        rowSwitch :: i -> i -> t -> t
        -- | Multiply a row by a number.
        rowMult :: (Num e, (Elem t) ~ e) => i -> (Elem t) -> t -> t
        -- | @'rowAdd' i1 a i2 t@ adds @a@ times the row @i2@ to the
        -- row @i1@ ot @t@.
        rowAdd :: (Num e, (Elem t) ~ e) => i -> (Elem t) -> i -> t -> t
        -- | Switch two columns.
        colSwitch :: j -> j -> t -> t
        -- | Multiply a column by a number.
        colMult :: (Num e, (Elem t) ~ e) => j -> (Elem t) -> t -> t
        -- | @'colAdd' j1 a j2 t@ adds @a@ times the column @j2@ to
        -- the column @j1@ ot @t@.
        colAdd :: (Num e, (Elem t) ~ e) => j -> (Elem t) -> j -> t -> t
        -- | Reduced row echelon form of the matrix.
        rowEchelonForm :: (Eq e, Fractional e, (Elem t) ~ e) => t -> t


-- | Solves linear systems @AX=B@; @t1@ is the type of @A@, @t2@ is
-- the type of @B@, and @'SolSpace' t1 t2@ is the type of the solution
-- @X@.
class LinearSystem t1 t2 where
    type SolSpace t1 t2
    -- | Performs row operations on the augmented matrix [t1,t2] until
    -- t1 is in reduced row echelon form, then slits the result.
    triangularSolve :: t1 -> t2 -> (t1,t2)
    -- | Returns @'Nothing'@ if the system @AX=B@ has no solution,
    -- otherwise returns a solution for the system and a list of basis
    -- vectors for the kernel of @A@.
    parametricSolve :: t1 -> t2 -> Maybe (SolSpace t1 t2,[SolSpace t1 t2])


class SquareMatrix t where
    -- | Indentity matrix.
    unit :: Num e => t e
    -- | Inverts, if the matrix is invertible, otherwise @'Nothing'@.
    inverse :: (Eq e, Fractional e) => t e -> Maybe (t e)
    -- | Trace of the matrix.
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
    -- | Minimal polynomial. The algorithm follows the paper of
    -- S. Bialas and M. Bialas
    -- <http://bulletin.pan.pl/(56-4)391.pdf>. The number of
    -- operations is O(n^4), where n is the number of rows of the
    -- matrix.
    minPoly :: (Eq e, Fractional e) => t e -> [e]
    -- | Determinant of the matrix.
    det :: Num e => t e -> e
    det = head . charPoly
    -- | Evaluate a polynomial on a matrix.
    polyEval :: Num e => t e -> [e] -> t e

