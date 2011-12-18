{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.Tensor where

import Data.HList
import Data.MultiIndex
import Data.Ordinal
import qualified Data.Vector as V

data MultiIndex i ⇒ Tensor i a = Tensor [Int] (V.Vector a)
                               deriving Eq

-- ottimizzare tail
instance (Show a) ⇒ Show (Tensor i a) where
    show (Tensor [] x) = show (x V.! 0)
    show (Tensor (i:[]) x) = "[" ++ showT x i ""
        where showT v n acc | n == 1 = acc ++ (show (v V.! 0)) ++ "]"
                            | n > 1 = showT (V.tail v) (n-1)
                                      (acc ++ (show (v V.! 0)) ++ ",")
    show (Tensor (i:is) x) = "[" ++ showT x (i:is) []
        where showT v (j:js) acc | j == 1 = acc ++ (show (Tensor js v)) ++ "]"
                                 | j > 1 = showT (V.drop (foldl1 (*) js) v)
                                           ((j-1):js)
                                           (acc ++ (show (Tensor js v)) ++  ",")

class FromVector e a | a → e where
    fromVector ∷ V.Vector e → a

instance (Bounded i, MultiIndex i) ⇒ FromVector e (Tensor i e) where
    fromVector x = toTensor maxBound x
        where toTensor ∷ MultiIndex i ⇒ i → V.Vector a → Tensor i a
              toTensor i v | (V.length v) < l = error ("Length of vector must be at least "
                                       ++ (show l))
                           | otherwise = Tensor (dimensions i) (V.take l v)
                  where l = card i


class FromList e a | a → e where
    fromList ∷ [e] → a

instance (Bounded i, MultiIndex i) ⇒ FromList a (Tensor i a) where
    fromList = fromVector . V.fromList

-- | In any instance of @'MultiIndexable'@ @'dims'@ should be
-- independent of its argument and work on @'undefined'@.
class (MultiIndex i) ⇒ MultiIndexable i e a | a → e, a → i where
    (!) ∷ a → i → e
    dims ∷ a → i

----
instance (Bounded i, MultiIndex i) ⇒ MultiIndexable i a (Tensor i a) where
    (Tensor d x) ! j = x V.! ((multiIndex2Linear j) - 1)
    dims _ = maxBound

type ColumnVector n a = Tensor (n :|: End) a

type Vector n a = ColumnVector n a

type RowVector n a = Tensor (One :|: (n :|: End)) a

type Matrix m n a = Tensor (m :|: (n :|: End)) a

class (Num a) ⇒ VectorSpace a v | v → a where
    zero ∷ v
    (*.) ∷ a → v → v
    (.+.) ∷ v → v → v
--    dimension ∷ v → Integer

instance (Bounded i, MultiIndex i, Num a) ⇒ VectorSpace a (Tensor i a) where
    zero = z
           where z = Tensor d $ V.replicate l 0
                 l = card $ dims (asTypeOf undefined z)
                 d = dimensions $ dims (asTypeOf undefined z)
    a *. (Tensor d v) = Tensor d (V.map  (* a) v)
    (Tensor d x) .+. (Tensor e y) = Tensor d (V.zipWith (+) x y)
--    dimension _ = dim (undefined :: i)

class (MultiIndex i) ⇒ Product n i a b c | a → n, b → n, i a b → c where
    prod ∷ i → a → b → c


--instance (Num a, MultiIndex j, HAppend i j k, HAppend j l m, HAppend i l n) ⇒
instance (Num a, MultiIndex j, DropAt j k i, TakeUntil j m l, HAppend i l n) ⇒
    Product a j (Tensor k a) (Tensor m a) (Tensor n a) where
        prod i (Tensor d1 x) (Tensor d2 y) =
            Tensor d (V.generate l genP)
                where lj = card i
                      ll = (V.length y) `div` lj
                      l = ((V.length x) `div` lj) * ll
                      genP = \n → mult ((n `div` ll)*lj) (n `mod` ll) 1 0
                      mult u v t acc | t <= lj = mult (u + 1) (v + ll) (t + 1)
                                                 ((x V.! u)*(y V.! v) + acc)
                                     | otherwise = acc
                      d = (take ((length d1) - (length $ dimensions i)) d1) ++
                          (drop (length $ dimensions i) d2)


class MatrixProduct n a b c | a b → c, a → n, b → n where
    (.*.) ∷ a → b → c

instance (Product a (m :|: End) t1 t2 t3, MultiIndexable i a t2, HHead i m) ⇒
    MatrixProduct a t1 t2 t3 where
        x .*. y = prod ((hHead $ dims y) :|: End) x y


class TensorProduct n a b c | a → n, b → n, a b → c where
    (⊗) ∷ a → b → c

instance (Num a, Product a End t1 t2 t3) ⇒ TensorProduct a t1 t2 t3 where
    (⊗) = prod End

class Transpose a b| a → b where
    transpose ∷ a → b


----- WRONG
instance (MReverse i j) ⇒ Transpose (Tensor i a) (Tensor j a) where
    transpose (Tensor d x) = Tensor (reverse d) (V.reverse x)

class DotProduct n a b c | a b → c, a → n, b → n where
    dot ∷ a → b → c

instance (MultiIndexable i a t1, MultiIndexable i a t2, Product a i t1 t2 t3) ⇒
    DotProduct a t1 t2 t3 where
        dot x y = prod (dims x) x y

class (Num a, Ordinal i, Ordinal j) ⇒
    RMatrix a i j m | m → a, m → i, m → j where
                        rowSwitch ∷ i → i → m → m
                        rowMult ∷ i → a → m → m
                        rowAdd ∷ i → a → i → m → m
                        colSwitch ∷ j → j → m → m
                        colMult ∷ j → a → m → m
                        colAdd ∷ j → a → j → m → m

instance (Num a, Ordinal i, Ordinal j) ⇒ RMatrix a i j (Tensor (i :|: (j :|: End)) a) where
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

class (Fractional a, Ordinal i, Ordinal j) ⇒ EchelonForm a i j m | m → a, m → i, m → j where
    rowEchelonForm ∷ m → m

class (Fractional a, Ordinal i, Ordinal j) ⇒ LinearSystem a i j m n | m → a, m → i, m → j, n → a, n → i where
    solveLinSystem ∷ m → n → (m,n)


instance (Fractional a, Ordinal i, Ordinal j) ⇒
    EchelonForm a i j (Tensor (i :|: (j :|: End)) a) where
        rowEchelonForm (Tensor [d1,d2] v)
            = Tensor [d1,d2] (rowEchelonOnVec d1 d2 0 v)

instance (Fractional a, Ordinal i, Ordinal j, Ordinal k) ⇒
    LinearSystem a i j (Tensor (i :|: (j :|: End)) a) (Tensor (i :|: (k :|: End)) a) where
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

-- | Row switch on Vector representation of the matrix
rowSwitchOnVec ∷ Int -- ^ First row to switsh
               → Int -- ^ Second row to switch
               → Int -- ^ Number of rows
               → Int -- ^ Number of columns
               → V.Vector a -- ^ Input Vector
               → V.Vector a -- ^ Output Vector
rowSwitchOnVec i1 i2 d e x = V.generate (d*e) rs
    where rs n | e*(i1 -1) <= n && n < e*i1 = x V.! (n + off)
               | e*(i2 -1) <= n && n < e*i2 = x V.! (n - off)
               | otherwise = x V.! n       
          off = e * (i2 - i1)

-- | Column switch on Vector representation of the matrix
colSwitchOnVec ∷ Int -- ^ First column to switsh
               → Int -- ^ Second column to switch
               → Int -- ^ Number of rows
               → Int -- ^ Number of columns
               → V.Vector a -- ^ Input Vector
               → V.Vector a -- ^ Output Vector
colSwitchOnVec j1 j2 d e x = V.generate (d*e) cs
    where cs n | rem n e == j1 - 1 = x V.! (n + off)
               | rem n e == j2 - 1 = x V.! (n - off)
               | otherwise = x V.! n
          off =  j2 - j1

-- | Row multiplication on Vector representation of the matrix
rowMultOnVec ∷ (Num a)
                 ⇒ Int -- ^ Row to multiply
                 → a -- ^ Multiplier
                 → Int -- ^ Number of rows
                 → Int -- ^ Number of columns
                 → V.Vector a -- ^ Input Vector
                 → V.Vector a -- ^ Output Vector
rowMultOnVec i a d e x = V.generate (d*e) (\n →
                                              if (e*(i - 1)) <= n && n < e*i
                                              then (x V.! n) * a
                                              else x V.! n
                                          )

-- | Column multiplication on Vector representation of the matrix
colMultOnVec ∷ (Num a)
                 ⇒ Int -- ^ Column to multiply
                 → a -- ^ Multiplier
                 → Int -- ^ Number of rows
                 → Int -- ^ Number of columns
                 → V.Vector a -- ^ Input Vector
                 → V.Vector a -- ^ Output Vector
colMultOnVec j a d e x = V.generate (d*e) (\n → if rem n e == j - 1
                                                then (x V.! n) * a
                                                else x V.! n
                                          )

-- | Row division on Vector representation of the matrix
rowDivOnVec ∷ (Fractional a)
                ⇒ Int -- ^ Row to divide
                → a -- ^ Divisor
                → Int -- ^ Number of rows
                → Int -- ^ Number of columns
                → V.Vector a -- ^ Input Vector
                → V.Vector a -- ^ Output Vector
rowDivOnVec i a d e x = V.generate (d*e) (\n →
                                             if (e*(i - 1)) <= n && n < e*i
                                             then (x V.! n) / a
                                             else x V.! n
                                         )

-- | Column division on Vector representation of the matrix
colDivOnVec ∷ (Fractional a)
                 ⇒ Int -- ^ Column to multiply
                 → a -- ^ Divisor
                 → Int -- ^ Number of rows
                 → Int -- ^ Number of columns
                 → V.Vector a -- ^ Input Vector
                 → V.Vector a -- ^ Output Vector
colDivOnVec j a d e x = V.generate (d*e) (\n → if rem n e == j - 1
                                                then (x V.! n) / a
                                                else x V.! n
                                          )

-- | Row add on Vector representation of the matrix
rowAddOnVec ∷ (Num a)
                ⇒ Int -- ^ Row we add to
                → a -- ^ How much of the row we wish to add
                → Int -- ^ Row we add
                → Int -- ^ Number of rows
                → Int -- ^ Number of columns
                → V.Vector a -- ^ Input Vector
                → V.Vector a -- ^ Output Vector
rowAddOnVec i1 a i2 d e x = V.generate (d*e) ra
    where ra n | e*(i1 -1) <= n && n < e*i1 = x V.! n + (x V.! (n + off))*a
               | otherwise = x V.! n
          off = e * (i2 - i1)

-- | Column add on Vector representation of the matrix
colAddOnVec ∷ (Num a)
                ⇒ Int -- ^ Column we add to
                → a -- ^ How much of the column we wish to add
                → Int -- ^ Column we add
                → Int -- ^ Number of rows
                → Int -- ^ Number of columns
                → V.Vector a -- ^ Input Vector
                → V.Vector a -- ^ Output Vector
colAddOnVec j1 a j2 d e x = V.generate (d*e) ca
    where ca n | rem n e == j1 - 1
                   = x V.! n + (x V.! (n + off))*a
               | otherwise = x V.! n
          off = j2 - j1

-- | Row subtract on Vector representation of the matrix
rowSubOnVec ∷ (Num a)
               ⇒ Int -- ^ Row we subtract to
            → a -- ^ How much of the row we wish to subtract
            → Int -- ^ Row we add
            → Int -- ^ Number of rows
            → Int -- ^ Number of columns
            → V.Vector a -- ^ Input Vector
            → V.Vector a -- ^ Output Vector
rowSubOnVec i1 a i2 d e x = V.generate (d*e) rs
    where rs n | e*(i1 -1) <= n && n < e*i1 = x V.! n - (x V.! (n + off))*a
               | otherwise = x V.! n
          off = e * (i2 - i1)

-- | Column subtract on Vector representation of the matrix
colSubOnVec ∷ (Num a)
                ⇒ Int -- ^ Column we subtract to
                → a -- ^ How much of the column we wish to subtract
                → Int -- ^ Column we subtract
                → Int -- ^ Number of rows
                → Int -- ^ Number of columns
                → V.Vector a -- ^ Input Vector
                → V.Vector a -- ^ Output Vector
colSubOnVec j1 a j2 d e x = V.generate (d*e) ca
    where ca n | rem n e == j1 - 1
                   = x V.! n - (x V.! (n + off))*a
               | otherwise = x V.! n
          off = j2 - j1

-- | Row echelon form on Vector representation of the matrix
rowEchelonOnVec ∷ (Fractional a)
                    ⇒ Int -- ^ Number of rows
                    → Int -- ^ Number of columns of the first matrix
                    → Int -- ^ Number of columns of the second matrix
                    → V.Vector a -- ^ Input Vector
                    → V.Vector a -- ^ Output Vector
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