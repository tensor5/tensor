{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

import           Data.MultiIndex
import           Data.Ordinal
import           Data.Tensor
import           Data.Tensor.Vector.Internal
import qualified Data.Vector as V


instance (Fractional e, Bounded i, Ordinal i) =>  SquareMatrix e i (Tensor (i :|: (i :|: End)) e) where
    unit = u
           where u = Tensor d $ V.generate (i*i) g
                 g n = if rem n (i + 1) == 0
                       then 1
                       else 0
                 i = head d
                 d = dimensions $ dims (asTypeOf undefined u)
    inverse m = let (f,s) = solveLinSystem m u in
                if f == u
                then Just s
                else Nothing
                    where u = asTypeOf unit m
    tr (Tensor [d,d'] x) = trace 0 0
        where trace i acc = if i < d*d
                            then trace (i + d + 1) (acc + (x V.! i))
                            else acc

instance (Fractional e, Ordinal i, Ordinal j) =>
    EchelonForm e i j (Tensor (i :|: (j :|: End)) e) where
        rowEchelonForm (Tensor [d1,d2] v)
            = Tensor [d1,d2] (rowEchelonOnVec d1 d2 0 v)

instance (Fractional e, Ordinal i, Ordinal j, Ordinal k) =>
    LinearSystem e i j (Tensor (i :|: (j :|: End)) e) (Tensor (i :|: (k :|: End)) e) where
        solveLinSystem (Tensor [d1,d2] v) (Tensor [d1',d3] w)
            = split $ rowEchelonOnVec d1 d2 d3 (cat v w)
              where cat x y  = V.generate (d1*(d2+d3)) gen
                        where gen n | rem n (d2+d3) < d2
                                        = x V.! ((quot n (d2+d3))*d2 +
                                                 (rem n (d2+d3)))
                                    | otherwise
                                        = y V.! ((quot n (d2+d3))*d3 +
                                                 (rem n (d2+d3)) - d2)
                    split z = (Tensor [d1,d2] (V.generate (d1*d2) a), Tensor [d1,d3] (V.generate (d1*d3) b))
                        where a n = z V.! ((quot n d2)*(d2+d3) + (rem n d2))
                              b n = z V.! ((quot n d3)*(d2+d3) + (rem n d3) + d2)


-- | Row echelon form on Vector representation of the matrix
rowEchelonOnVec ∷ (Fractional a)
                    => Int -- ^ Number of rows
                    -> Int -- ^ Number of columns of the first matrix
                    -> Int -- ^ Number of columns of the second matrix
                    -> V.Vector a -- ^ Input Vector
                    -> V.Vector a -- ^ Output Vector
rowEchelonOnVec d e1 e2 x = re 1 1 2 x
    where re i j i' x | i > d || j > e1 = x
                      | x V.! (coor i j) == 0 =
                          if i' <= d
                          then re i j (i'+1) (rowSwitchOnVec i i' d (e1+e2) x)
                          else re i (j+1) (i+1) x
                      | otherwise = re (i+1) (j+1) (i+2)
                                    (rEl i 1 j (rowDivOnVec i (x V.! (coor i j)) d (e1+e2) x))
          -- Assuming the (i,j) element of the matrix is 1, makes
          -- 0 all the other elements in the j-th column
          rEl i i' j x | i' > d = x
                       | i' == i = rEl i (i'+1) j x
                       | otherwise = rEl i (i'+1) j (rowSubOnVec i' (x V.! (coor i' j)) i d (e1+e2) x)
          coor i j = (i - 1)*(e1+e2) + j - 1