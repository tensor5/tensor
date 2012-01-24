{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tensor.LinearAlgebra.Vector
    (
     module Data.Tensor.LinearAlgebra
    ) where

import           Data.Cardinal hiding (Succ)
import qualified Data.Cardinal as C
import           Data.TypeList.MultiIndex hiding (take, drop, length)
import           Data.Ordinal
import           Data.Tensor.LinearAlgebra
import           Data.Tensor.Vector hiding (Matrix)
import qualified Data.Tensor.Vector as TV
import           Data.Tensor.Vector.Internal hiding (Matrix)
import qualified Data.Vector as V
import           Prelude hiding (replicate, zipWith)

instance (Bounded i, Cardinality i, MultiIndex i) =>
    VectorSpace (Tensor i) where
        zero = replicate 0
        a *. t = fmap (* a) t
        (.+.) = zipWith (+)
--    dimension _ = dim (undefined :: i)


instance (Num e, Cardinal n, MultiIndex i, MultiIndex j, JoinList n i j) =>
    Product n (Tensor i e) (Tensor j e) where
        type ProdSpace n (Tensor i e) (Tensor j e) = Tensor (Join n i j) e
        prod n (Tensor d1 x) (Tensor d2 y) =
            Tensor d (V.generate l genP)
                where lj = product (take (fromCardinal n) d2)
                      ll = V.length y `div` lj
                      l = (V.length x `div` lj) * ll
                      genP m = mult ((m `div` ll) * lj) (m `mod` ll) 1 0
                      mult u v t acc | t <= lj = mult (u + 1) (v + ll) (t + 1)
                                                 ((x V.! u)*(y V.! v) + acc)
                                     | otherwise = acc
                      d = (take (length d1 - fromCardinal n) d1) ++
                          (drop (fromCardinal n) d2)


instance (Product (C.Succ Zero) t1 t2) => MatrixProduct t1 t2 where
    type MatrixProductSpace t1 t2 = ProdSpace (C.Succ Zero) t1 t2
    x .*. y = prod (undefined :: C1) x y


instance (Product C0 t1 t2) => TensorProduct t1 t2 where
    type t1 :⊗: t2 = ProdSpace C0 t1 t2
    (⊗) = prod (undefined :: C0)


instance DotProduct (Tensor i) where
    dot (Tensor _ x) (Tensor _ y) = V.sum $ V.zipWith (*) x y


instance (Num e, Ordinal i, Ordinal j) => Matrix e i j (Tensor (i :|: (j :|: Nil)) e) where
    rowSwitch i1 i2 (Tensor ds v) | i1 /= i2 = Tensor ds (rowSwitchOnVec
                                                          (fromOrdinal i1)
                                                          (fromOrdinal i2)
                                                          (head ds)
                                                          (head $ tail ds)
                                                          v
                                                         )
                                  | otherwise = Tensor ds v
    colSwitch j1 j2 (Tensor ds v) | j1 /= j2 = Tensor ds (colSwitchOnVec
                                                          (fromOrdinal j1)
                                                          (fromOrdinal j2)
                                                          (head ds)
                                                          (head $ tail ds)
                                                          v
                                                         )
                                  | otherwise = Tensor ds v
    rowMult i a (Tensor ds v) = Tensor ds (rowMultOnVec
                                           (fromOrdinal i)
                                           a
                                           (head ds)
                                           (head $ tail ds)
                                           v
                                          )
    colMult j a (Tensor ds v) = Tensor ds (colMultOnVec
                                           (fromOrdinal j)
                                           a
                                           (head ds)
                                           (head $ tail ds)
                                           v
                                          )
    rowAdd i1 a i2 (Tensor ds v) = Tensor ds (rowAddOnVec
                                              (fromOrdinal i1)
                                              a
                                              (fromOrdinal i2)
                                              (head ds)
                                              (head $ tail ds)
                                              v
                                             )
    colAdd j1 a j2 (Tensor ds v) = Tensor ds (colAddOnVec
                                              (fromOrdinal j1)
                                              a
                                              (fromOrdinal j2)
                                              (head ds)
                                              (head $ tail ds)
                                              v
                                             )


-- | Row switch on Vector representation of the matrix
rowSwitchOnVec :: Int -- ^ First row to switsh
               -> Int -- ^ Second row to switch
               -> Int -- ^ Number of rows
               -> Int -- ^ Number of columns
               -> V.Vector a -- ^ Input Vector
               -> V.Vector a -- ^ Output Vector
rowSwitchOnVec i1 i2 d e x = V.generate (d*e) rs
    where rs n | e*(i1 -1) <= n && n < e*i1 = x V.! (n + off)
               | e*(i2 -1) <= n && n < e*i2 = x V.! (n - off)
               | otherwise = x V.! n
          off = e * (i2 - i1)

-- | Column switch on Vector representation of the matrix
colSwitchOnVec :: Int -- ^ First column to switch
               -> Int -- ^ Second column to switch
               -> Int -- ^ Number of rows
               -> Int -- ^ Number of columns
               -> V.Vector a -- ^ Input Vector
               -> V.Vector a -- ^ Output Vector
colSwitchOnVec j1 j2 d e x = V.generate (d*e) cs
    where cs n | rem n e == j1 - 1 = x V.! (n + off)
               | rem n e == j2 - 1 = x V.! (n - off)
               | otherwise = x V.! n
          off =  j2 - j1

-- | Row multiplication on Vector representation of the matrix
rowMultOnVec :: Num a
             => Int -- ^ Row to multiply
             -> a -- ^ Multiplier
             -> Int -- ^ Number of rows
             -> Int -- ^ Number of columns
             -> V.Vector a -- ^ Input Vector
             -> V.Vector a -- ^ Output Vector
rowMultOnVec i a d e x = V.generate (d*e) (\n ->
                                              if (e*(i - 1)) <= n && n < e*i
                                              then (x V.! n) * a
                                              else x V.! n
                                          )

-- | Column multiplication on Vector representation of the matrix
colMultOnVec :: Num a
             => Int -- ^ Column to multiply
             -> a -- ^ Multiplier
             -> Int -- ^ Number of rows
             -> Int -- ^ Number of columns
             -> V.Vector a -- ^ Input Vector
             -> V.Vector a -- ^ Output Vector
colMultOnVec j a d e x = V.generate (d*e) (\n -> if rem n e == j - 1
                                                 then (x V.! n) * a
                                                 else x V.! n
                                          )

-- | Row division on Vector representation of the matrix
rowDivOnVec :: Fractional a
            => Int -- ^ Row to divide
            -> a -- ^ Divisor
            -> Int -- ^ Number of rows
            -> Int -- ^ Number of columns
            -> V.Vector a -- ^ Input Vector
            -> V.Vector a -- ^ Output Vector
rowDivOnVec i a d e x = V.generate (d*e) (\n ->
                                             if (e*(i - 1)) <= n && n < e*i
                                             then (x V.! n) / a
                                             else x V.! n
                                         )

-- | Column division on Vector representation of the matrix
colDivOnVec :: Fractional a
            => Int -- ^ Column to multiply
            -> a -- ^ Divisor
            -> Int -- ^ Number of rows
            -> Int -- ^ Number of columns
            -> V.Vector a -- ^ Input Vector
            -> V.Vector a -- ^ Output Vector
colDivOnVec j a d e x = V.generate (d*e) (\n -> if rem n e == j - 1
                                                then (x V.! n) / a
                                                else x V.! n
                                          )

-- | Row add on Vector representation of the matrix
rowAddOnVec :: Num a
            => Int -- ^ Row we add to
            -> a -- ^ How much of the row we wish to add
            -> Int -- ^ Row we add
            -> Int -- ^ Number of rows
            -> Int -- ^ Number of columns
            -> V.Vector a -- ^ Input Vector
            -> V.Vector a -- ^ Output Vector
rowAddOnVec i1 a i2 d e x = V.generate (d*e) ra
    where ra n | e*(i1 -1) <= n && n < e*i1 = x V.! n + (x V.! (n + off))*a
               | otherwise = x V.! n
          off = e * (i2 - i1)

-- | Column add on Vector representation of the matrix
colAddOnVec :: Num a
            => Int -- ^ Column we add to
            -> a -- ^ How much of the column we wish to add
            -> Int -- ^ Column we add
            -> Int -- ^ Number of rows
            -> Int -- ^ Number of columns
            -> V.Vector a -- ^ Input Vector
            -> V.Vector a -- ^ Output Vector
colAddOnVec j1 a j2 d e x = V.generate (d*e) ca
    where ca n | rem n e == j1 - 1
                   = x V.! n + (x V.! (n + off))*a
               | otherwise = x V.! n
          off = j2 - j1

-- | Row subtract on Vector representation of the matrix
rowSubOnVec :: Num a
            => Int -- ^ Row we subtract to
            -> a -- ^ How much of the row we wish to subtract
            -> Int -- ^ Row we add
            -> Int -- ^ Number of rows
            -> Int -- ^ Number of columns
            -> V.Vector a -- ^ Input Vector
            -> V.Vector a -- ^ Output Vector
rowSubOnVec i1 a i2 d e x = V.generate (d*e) rs
    where rs n | e*(i1 -1) <= n && n < e*i1 = x V.! n - (x V.! (n + off))*a
               | otherwise = x V.! n
          off = e * (i2 - i1)

-- | Column subtract on Vector representation of the matrix
colSubOnVec :: Num a
            => Int -- ^ Column we subtract to
            -> a -- ^ How much of the column we wish to subtract
            -> Int -- ^ Column we subtract
            -> Int -- ^ Number of rows
            -> Int -- ^ Number of columns
            -> V.Vector a -- ^ Input Vector
            -> V.Vector a -- ^ Output Vector
colSubOnVec j1 a j2 d e x = V.generate (d*e) ca
    where ca n | rem n e == j1 - 1
                   = x V.! n - (x V.! (n + off))*a
               | otherwise = x V.! n
          off = j2 - j1


instance (Fractional e, Bounded i, Ordinal i, Sum i i) =>  SquareMatrix e i (Tensor (i :|: (i :|: Nil)) e) where
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
    tr (Tensor ds x) = traceOnVec (head ds) x
    charPoly x | d == 1 = [tr x]
               | otherwise = go
                             (initClow x)
                             1
                             1
                             [tr x]
        where go v l s acc | l == d - 1 = (s * endClow x v) : acc
                           | otherwise = go
                                         (clowStep x v)
                                         (l+1)
                                         (negate s)
                                         ((s * endClow x v):acc)
              d = head $ form x


instance (Fractional e, Ordinal i, Ordinal j) =>
    EchelonForm e i j (Tensor (i :|: (j :|: Nil)) e) where
        rowEchelonForm (Tensor ds v)
            = Tensor ds (rowEchelonOnVec (head ds) (head $ tail ds) 0 v)


instance (Fractional e, Ordinal i, Ordinal j, Ordinal k, Sum j k) =>
    LinearSystem e i j (Tensor (i :|: (j :|: Nil)) e) (Tensor (i :|: (k :|: Nil)) e) where
        solveLinSystem m1 m2
            = let (Tensor [d1,_] x) = directSum (undefined :: C1) m1 m2
                  (Tensor [_,d2] _) = m1
                  (Tensor [_,d3] _) = m2 in
              split d1 d2 d3 $ rowEchelonOnVec d1 d2 d3 x
              where split d1 d2 d3 z = (Tensor [d1,d2] (V.generate (d1*d2) a), Tensor [d1,d3] (V.generate (d1*d3) b))
                        where a n = z V.! ((quot n d2)*(d2+d3) + (rem n d2))
                              b n = z V.! ((quot n d3)*(d2+d3) + (rem n d3) + d2)


-- | Row echelon form on Vector representation of the matrix
rowEchelonOnVec :: (Fractional a)
                    => Int -- ^ Number of rows
                    -> Int -- ^ Number of columns of the first matrix
                    -> Int -- ^ Number of columns of the second matrix
                    -> V.Vector a -- ^ Input Vector
                    -> V.Vector a -- ^ Output Vector
rowEchelonOnVec d e1 e2 v = re 1 1 2 v
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

traceOnVec :: (Num a) =>
              Int -- ^ Number of rows (or columns)
           -> V.Vector a -- ^ Vector representation of the matrix
           -> a
traceOnVec d x = trace 0 0
    where trace i acc = if i < d*d
                        then trace (i + d + 1) (acc + (x V.! i))
                        else acc


initClow :: Num e =>
            TV.Matrix i i e -- ^ Input matrix
         -> TV.Matrix i i e -- ^ Matrix of length 2 clows
initClow x = unsafeMatrixGen d d g
    where g c0 c' | c0 < c' = a c0 c'
                  | c0 == c' = negate $ sum [ a c'' c'' | c'' <- [1 .. c'-1]]
                  | otherwise = 0
          a i j = unsafeMatrixGet i j x
          d = head $ form x


-- | Makes one more step in the clow sequence
clowStep :: Num e =>
            TV.Matrix i i e -- ^ Input matrix
         -> TV.Matrix i i e -- ^ Input clow nodes of length l
         -> TV.Matrix i i e -- ^ Output clow nodes of length l+1
clowStep x y = unsafeMatrixGen d d g
    where g c0 c' | c0 < c' = sum [(b c0 c)*(a c c') | c <- [c0 .. d]]
                  | c0 == c' = negate $ sum [(b c''  c)*(a c c'') | c'' <- [1 .. c'-1], c <- [c'' .. d]]
                  | otherwise = 0
          a i j = unsafeMatrixGet i j x
          b i j = unsafeMatrixGet i j y
          d = head $ form x


endClow :: Num e =>
           TV.Matrix i i e -- ^ Input matrix
        -> TV.Matrix i i e -- ^ Input clow nodes of length l
        -> e
endClow x y = negate $ sum [(b c''  c)*(a c c'') | c'' <- [1 .. d], c <- [c'' .. d]]
    where a i j = unsafeMatrixGet i j x
          b i j = unsafeMatrixGet i j y
          d = head $ form x

