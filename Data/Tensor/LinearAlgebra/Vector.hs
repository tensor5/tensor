{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.Tensor.LinearAlgebra.Vector where

import           Data.Cardinal hiding (Succ)
import qualified Data.Cardinal as C
import           Data.TypeList.MultiIndex hiding (take, drop, length)
import           Data.Ordinal
import           Data.Tensor.LinearAlgebra.Common
import           Data.Tensor.Vector
import           Data.Tensor.Vector.Internal
import           Data.TypeAlgebra
import qualified Data.Vector as V
import           Prelude hiding (zipWith)

instance (Bounded i, Cardinality i, MultiIndex i) =>
    VectorSpace (Tensor i) where
        zero = z
            where z = Tensor d $ V.replicate l 0
                  l = card $ dims (asTypeOf undefined z)
                  d = dimensions $ dims (asTypeOf undefined z)
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
                      genP n = mult ((n `div` ll) * lj) (n `mod` ll) 1 0
                      mult u v t acc | t <= lj = mult (u + 1) (v + ll) (t + 1)
                                                 ((x V.! u)*(y V.! v) + acc)
                                     | otherwise = acc
                      d = (take (length d1 - fromCardinal n) d1) ++
                          (drop (fromCardinal n) d2)
{-
instance (Num a, MultiIndex j, DropAt j k i, TakeUntil j m l, HAppend i l n) =>
    Product a j (Tensor k a) (Tensor m a) (Tensor n a) where
        prod i (Tensor d1 x) (Tensor d2 y) =
            Tensor d (V.generate l genP)
                where lj = card i
                      ll = V.length y `div` lj
                      l = (V.length x `div` lj) * ll
                      genP n = mult ((n `div` ll) * lj) (n `mod` ll) 1 0
                      mult u v t acc | t <= lj = mult (u + 1) (v + ll) (t + 1)
                                                 ((x V.! u)*(y V.! v) + acc)
                                     | otherwise = acc
                      d = (take (length d1 - length (dimensions i::[Int])) d1)
                          ++ (drop (length (dimensions i::[Int])) d2)
-}

instance (Product (C.Succ Zero) t1 t2) => MatrixProduct t1 t2 where
    type MatrixProductSpace t1 t2 = ProdSpace (C.Succ Zero) t1 t2
    x .*. y = prod (undefined :: C1) x y


instance (Product C0 t1 t2) => TensorProduct t1 t2 where
    type t1 :⊗: t2 = ProdSpace C0 t1 t2
    (⊗) = prod (undefined :: C0)

{-
instance (Num e, TypeList i, Cardinal (Length i), MultiIndexable i e t, Product e (Length i) t t, MultiIndexable Nil e (ProdSpace (Length i) t t)) =>
    DotProduct e t where
        dot x y = (prod (TL.length $ dims x) x y) ! Nil
-}

instance DotProduct (Tensor i) where
    dot (Tensor _ x) (Tensor _ y) = V.sum $ V.zipWith (*) x y

{-
instance (Num e, TypeList i, Cardinal (Length i), MultiIndexable i e t1, MultiIndexable i e t2, Product e (Length i) t1 t2, MultiIndexable Nil e (ProdSpace (Length i) t1 t2)) => DotProduct e t1 t2 where
    dot x y = (prod (TL.length $ dims x) x y) ! Nil
-}
{-
instance (TypeList i, MultiIndexable i a t1, MultiIndexable i a t2, Product a (Length i) t1 t2) =>
    DotProduct a t1 t2 where
        dot x y = prod (dims x) x y
-}

instance (Num e, Ordinal i, Ordinal j) => RMatrix e i j (Tensor (i :|: (j :|: Nil)) e) where
    rowSwitch i1 i2 (Tensor [d1,d2] v) | i1 /= i2 =
                                           Tensor [d1,d2] (rowSwitchOnVec
                                                           (fromOrdinal i1)
                                                           (fromOrdinal i2)
                                                           d1
                                                           d2
                                                           v
                                                          )
                                       | otherwise = Tensor [d1,d2] v
    colSwitch j1 j2 (Tensor [d1,d2] v) | j1 /= j2 =
                                           Tensor [d1,d2] (colSwitchOnVec
                                                           (fromOrdinal j1)
                                                           (fromOrdinal j2)
                                                           d1
                                                           d2
                                                           v
                                                          )
                                       | otherwise = Tensor [d1,d2] v
    rowMult i a (Tensor [d1,d2] v) = Tensor [d1,d2]
                                     (rowMultOnVec (fromOrdinal i) a d1 d2 v)
    colMult j a (Tensor [d1,d2] v) = Tensor [d1,d2]
                                     (colMultOnVec (fromOrdinal j) a d1 d2 v)
    rowAdd i1 a i2 (Tensor [d1,d2] v) = Tensor [d1,d2] (rowAddOnVec
                                                        (fromOrdinal i1)
                                                        a
                                                        (fromOrdinal i2)
                                                        d1
                                                        d2
                                                        v
                                                       )
    colAdd j1 a j2 (Tensor [d1,d2] v) = Tensor [d1,d2] (colAddOnVec
                                                        (fromOrdinal j1)
                                                        a
                                                        (fromOrdinal j2)
                                                        d1
                                                        d2
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


instance (Fractional e, Bounded i, Ordinal i, Cardinal (Card i :*: Card i)) =>  SquareMatrix e i (Tensor (i :|: (i :|: Nil)) e) where
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
    tr (Tensor [d,_] x) = traceOnVec d x
    charPoly (Tensor [d,_] x) | d == 1 = [traceOnVec d x]
                               | otherwise = go (initClowOnVec d x) 1 1 [traceOnVec d x]
        where go v l s acc | l == d - 1 = (s * endClowOnVec d x v) : acc
                           | otherwise = go (clowStepOnVec d x v) (l+1) (negate s)
                                       ((s * endClowOnVec d x v):acc)
    det = head . charPoly


instance (Fractional e, Ordinal i, Ordinal j) =>
    EchelonForm e i j (Tensor (i :|: (j :|: Nil)) e) where
        rowEchelonForm (Tensor [d1,d2] v)
            = Tensor [d1,d2] (rowEchelonOnVec d1 d2 0 v)


instance (Fractional e, Ordinal i, Ordinal j, Ordinal k) =>
    LinearSystem e i j (Tensor (i :|: (j :|: Nil)) e) (Tensor (i :|: (k :|: Nil)) e) where
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

traceOnVec :: (Num a) =>
              Int -- ^ Number of rows (or columns)
           -> V.Vector a -- ^ Vector representation of the matrix
           -> a
traceOnVec d x = trace 0 0
    where trace i acc = if i < d*d
                        then trace (i + d + 1) (acc + (x V.! i))
                        else acc

-- | Makes one more step in the clow sequence
clowStepOnVec :: Num a =>
                 Int -- ^ Number of rows (or columns)
              -> V.Vector a -- ^ Vector representation of the matrix
              -> V.Vector a -- ^ Input clow nodes of length l
              -> V.Vector a -- ^ Output clow nodes of length l+1
clowStepOnVec d x y = generateMatrixOnVec d d g
    where g c0 c' | c0 < c' = sum [(b c0 c)*(a c c') | c <- [c0 .. d]]
                  | c0 == c' = negate $ sum [(b c''  c)*(a c c'') | c'' <- [1 .. c'-1], c <- [c'' .. d]]
                  | c0 > c' = 0
          a i j = getMatrixEntryOnVec d d i j x
          b i j = getMatrixEntryOnVec d d i j y

endClowOnVec :: Num a =>
             Int -- ^ Number of rows (or columns)
          -> V.Vector a -- ^ Vector representation of the matrix
          -> V.Vector a -- ^ Input clow nodes of length l
          -> a
endClowOnVec d x y = negate $ sum [(b c''  c)*(a c c'') | c'' <- [1 .. d], c <- [c'' .. d]]
    where a i j = getMatrixEntryOnVec d d i j x
          b i j = getMatrixEntryOnVec d d i j y

initClowOnVec :: Num a =>
             Int -- ^ Number of rows (or columns)
          -> V.Vector a -- ^ Vector representation of the matrix
          -> V.Vector a
initClowOnVec d x = generateMatrixOnVec d d g
    where g c0 c' | c0 < c' = a c0 c'
                  | c0 == c' = negate $ sum [ a c'' c'' | c'' <- [1 .. c'-1]]
                  | otherwise = 0
          a i j = getMatrixEntryOnVec d d i j x
