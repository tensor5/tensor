{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tensor.Vector.Internal where

import           Data.HList
import           Data.MultiIndex
import           Data.Ordinal
import           Data.Tensor
import qualified Data.Vector as V

data Tensor i e = Tensor [Int] (V.Vector e)
                  deriving Eq

instance Show e => Show (Tensor i e) where
    showsPrec _ = showsT
        where
          showsT (Tensor [] v) = shows $ v V.! 0
          showsT (Tensor [_] v) = shows (V.toList v)
          showsT (Tensor (1:is) v) = showsT (Tensor is v) . (']':)
          showsT (Tensor (i:is) v) | V.null v = id
                                   | otherwise = ('[':) . showsT (Tensor is x) .
                                                 (',':) .
                                                 showsT (Tensor ((i-1):is) xs)
              where
                n = product is
                (x,xs) = V.splitAt n v


instance Functor (Tensor i) where
    fmap f (Tensor is v) = Tensor is (fmap f v)


class FromVector e t | t -> e where
    fromVector :: V.Vector e -> t


instance (Bounded i, MultiIndex i) => FromVector e (Tensor i e) where
    fromVector x = toTensor maxBound x
        where
          toTensor :: MultiIndex i => i -> V.Vector e -> Tensor i e
          toTensor i v | V.length v < l = error ("fromVector: length of vector \
                                                 \must be at least " ++ show l)
                       | otherwise = Tensor (dimensions i) (V.take l v)
                       where l = card i


instance (Bounded i, MultiIndex i) => FromList a (Tensor i a) where
    fromList = fromVector . V.fromList


instance (Bounded i, MultiIndex i) => MultiIndexable i e (Tensor i e) where
    (Tensor _ x) ! j = x V.! (multiIndex2Linear j - 1)
    dims _ = maxBound


type ColumnVector n = Tensor (n :|: End)


type Vector n = ColumnVector n


type RowVector n = Tensor (One :|: (n :|: End))


type Matrix m n = Tensor (m :|: (n :|: End))


instance (HNat2Integral n, MultiIndex i, MultiIndex j, MultiIndexConcat n i j)
    => DirectSummable n (Tensor i e) (Tensor j e) e where
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


instance (Bounded i, MultiIndex i, Num e) => VectorSpace e (Tensor i e) where
    zero = z
           where z = Tensor d $ V.replicate l 0
                 l = card $ dims (asTypeOf undefined z)
                 d = dimensions $ dims (asTypeOf undefined z)
    a *. (Tensor d v) = Tensor d (V.map  (* a) v)
    (Tensor d x) .+. (Tensor _ y) = Tensor d (V.zipWith (+) x y)
--    dimension _ = dim (undefined :: i)


--instance (Num a, MultiIndex j, HAppend i j k, HAppend j l m, HAppend i l n) =>
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
                      d = (take (length d1 - length (dimensions i)) d1) ++
                          (drop (length $ dimensions i) d2)


instance (Product e (m :|: End) t1 t2 t3, MultiIndexable i e t2, HHead i m) =>
    MatrixProduct e t1 t2 t3 where
        x .*. y = prod ((hHead $ dims y) :|: End) x y


instance (Num e, Product e End t1 t2 t3) => TensorProduct e t1 t2 t3 where
    (⊗) = prod End


instance (Ordinal i, Ordinal j) =>
    Transpose (Tensor (i :|: (j :|: End)) a) (Tensor (j :|: (i :|: End)) a)
        where
          transpose (Tensor [d1,d2] x) = Tensor [d2,d1] (V.generate (d1*d2) tr)
              where tr n = let (q,r) = quotRem n d1
                           in x V.! (r * d2 + q)


instance (MultiIndexable i a t1, MultiIndexable i a t2, Product a i t1 t2 t3) =>
    DotProduct a t1 t2 t3 where
        dot x y = prod (dims x) x y


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

