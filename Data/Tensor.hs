{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.Tensor where

import           Data.HList
import           Data.MultiIndex
import           Data.Ordinal
import qualified Data.Vector as V

data MultiIndex i => Tensor i e = Tensor [Int] (V.Vector e)
                               deriving Eq

-- ottimizzare tail
instance Show e => Show (Tensor i e) where
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

instance Functor (Tensor i) where
    fmap f (Tensor is v) = Tensor is (fmap f v)


class FromVector e t | t -> e where
    fromVector ∷ V.Vector e -> t

instance (Bounded i, MultiIndex i) => FromVector e (Tensor i e) where
    fromVector x = toTensor maxBound x
        where toTensor ∷ MultiIndex i => i -> V.Vector a -> Tensor i a
              toTensor i v | (V.length v) < l = error ("Length of vector must be at least "
                                       ++ (show l))
                           | otherwise = Tensor (dimensions i) (V.take l v)
                  where l = card i


class FromList e t | t -> e where
    fromList ∷ [e] -> t

instance (Bounded i, MultiIndex i) => FromList a (Tensor i a) where
    fromList = fromVector . V.fromList

-- | In any instance of @'MultiIndexable'@ @'dims'@ should be
-- independent of its argument and work on @'undefined'@.
class MultiIndex i => MultiIndexable i e t | t -> e, t -> i where
    (!) ∷ t -> i -> e
    dims ∷ t -> i

----
instance (Bounded i, MultiIndex i) => MultiIndexable i e (Tensor i e) where
    (Tensor d x) ! j = x V.! (multiIndex2Linear j - 1)
    dims _ = maxBound

type ColumnVector n a = Tensor (n :|: End) a

type Vector n a = ColumnVector n a

type RowVector n a = Tensor (One :|: (n :|: End)) a

type Matrix m n a = Tensor (m :|: (n :|: End)) a


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

instance (Bounded i, MultiIndex i, Num e) => VectorSpace e (Tensor i e) where
    zero = z
           where z = Tensor d $ V.replicate l 0
                 l = card $ dims (asTypeOf undefined z)
                 d = dimensions $ dims (asTypeOf undefined z)
    a *. (Tensor d v) = Tensor d (V.map  (* a) v)
    (Tensor d x) .+. (Tensor e y) = Tensor d (V.zipWith (+) x y)
--    dimension _ = dim (undefined :: i)

class (MultiIndex i) => Product e i t1 t2 t3 | t1 -> e, t2 -> e, i t1 t2 -> e where
    prod ∷ i -> t1 -> t2 -> t3


--instance (Num a, MultiIndex j, HAppend i j k, HAppend j l m, HAppend i l n) =>
instance (Num a, MultiIndex j, DropAt j k i, TakeUntil j m l, HAppend i l n) =>
    Product a j (Tensor k a) (Tensor m a) (Tensor n a) where
        prod i (Tensor d1 x) (Tensor d2 y) =
            Tensor d (V.generate l genP)
                where lj = card i
                      ll = (V.length y) `div` lj
                      l = ((V.length x) `div` lj) * ll
                      genP = \n -> mult ((n `div` ll)*lj) (n `mod` ll) 1 0
                      mult u v t acc | t <= lj = mult (u + 1) (v + ll) (t + 1)
                                                 ((x V.! u)*(y V.! v) + acc)
                                     | otherwise = acc
                      d = (take ((length d1) - (length $ dimensions i)) d1) ++
                          (drop (length $ dimensions i) d2)


class MatrixProduct e t1 t2 t3 | t1 t2 -> t3, t1 -> t2, t2 -> e where
    (.*.) ∷ t1 -> t2 -> t3

instance (Product e (m :|: End) t1 t2 t3, MultiIndexable i e t2, HHead i m) =>
    MatrixProduct e t1 t2 t3 where
        x .*. y = prod ((hHead $ dims y) :|: End) x y


class TensorProduct e t1 t2 t3 | t1 -> e, t2 -> e, t1 t2 -> t3 where
    (⊗) ∷ t1 -> t2 -> t3

instance (Num e, Product e End t1 t2 t3) => TensorProduct e t1 t2 t3 where
    (⊗) = prod End

class Transpose t1 t2 | t1 -> t2 where
    transpose ∷ t1 -> t2

instance (Ordinal i, Ordinal j) =>
    Transpose (Tensor (i :|: (j :|: End)) a) (Tensor (j :|: (i :|: End)) a)
        where
          transpose (Tensor [d1,d2] x) = Tensor [d2,d1] (V.generate (d1*d2) tr)
              where tr n = x V.! ((rem n d1)*d2 + (quot n d1))

class DotProduct e a b c | a b -> c, a -> e, b -> e where
    dot ∷ a -> b -> c

instance (MultiIndexable i a t1, MultiIndexable i a t2, Product a i t1 t2 t3) =>
    DotProduct a t1 t2 t3 where
        dot x y = prod (dims x) x y

class (Num e, Ordinal i, Ordinal j) =>
    RMatrix e i j t |  t -> e, t -> i, t -> j where
                        rowSwitch ∷ i -> i -> t -> t
                        rowMult ∷ i -> e -> t -> t
                        rowAdd ∷ i -> e -> i -> t -> t
                        colSwitch ∷ j -> j -> t -> t
                        colMult ∷ j -> e -> t -> t
                        colAdd ∷ j -> e -> j -> t -> t

instance (Num e, Ordinal i, Ordinal j) => RMatrix e i j (Tensor (i :|: (j :|: End)) e) where
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

class (Fractional e, Ordinal i, Ordinal j) => EchelonForm e i j t | t -> e, t -> i, t -> j where
    rowEchelonForm ∷ t -> t

class (Fractional e, Ordinal i, Ordinal j) => LinearSystem e i j t1 t2 | t1 -> e, t1 -> i, t1 -> j, e -> t1, e -> i where
    solveLinSystem ∷ t1 -> t2 -> (t1,t2)


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

class (Fractional e, Ordinal i) => SquareMatrix e i t | t -> e, t -> i where
    unit ∷ t
    inverse ∷ t -> Maybe t
    tr ∷ t -> e

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

-- | Row switch on Vector representation of the matrix
rowSwitchOnVec ∷ Int -- ^ First row to switsh
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
colSwitchOnVec ∷ Int -- ^ First column to switsh
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
rowMultOnVec ∷ (Num a)
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
colMultOnVec ∷ (Num a)
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
rowDivOnVec ∷ (Fractional a)
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
colDivOnVec ∷ (Fractional a)
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
rowAddOnVec ∷ (Num a)
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
colAddOnVec ∷ (Num a)
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
rowSubOnVec ∷ (Num a)
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
colSubOnVec ∷ (Num a)
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