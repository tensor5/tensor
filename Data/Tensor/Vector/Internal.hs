{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tensor.Vector.Internal where

import           Data.Cardinal
import           Data.TypeList.MultiIndex hiding (drop, take)
import           Data.TypeList.MultiIndex.Internal
import           Data.Ordinal
import           Data.Tensor
import qualified Data.Vector as V
import           Text.Show

data Tensor i e = Tensor [Int] (V.Vector e)
                  deriving Eq


type ColumnVector n = Tensor (n :|: Nil)


type Vector n = ColumnVector n


type RowVector n = Tensor (One :|: (n :|: Nil))


type Matrix m n = Tensor (m :|: (n :|: Nil))


instance Show e => Show (Tensor i e) where
    showsPrec _ = showsT
        where
          showsT (Tensor [] v) = shows $ v V.! 0
          showsT (Tensor [_] v) = showListWith shows (V.toList v)
          showsT (Tensor (1:is) v) = showsT (Tensor is v) . (']':)
          showsT (Tensor (i:is) v) = ('[':) . showsT (Tensor is x) . (',':) .
                                     showsT (Tensor ((i-1):is) xs)
              where
                n = product is
                (x,xs) = V.splitAt n v


instance Functor (Tensor i) where
    fmap f (Tensor is v) = Tensor is (fmap f v)


instance Zip (Tensor i) where
    zipWith f (Tensor d x) (Tensor _ y) = Tensor d $ V.zipWith f x y



class FromVector t where
    fromVector :: V.Vector e -> t e


instance (Bounded i, Cardinality i, MultiIndex i) =>
    FromVector (Tensor i) where
        fromVector x = toTensor maxBound x
            where
              toTensor :: (Cardinality i, MultiIndex i) =>
                          i -> V.Vector e -> Tensor i e
              toTensor i v | V.length v < l = error ("fromVector: length of vector \
                                                     \must be at least " ++ show l)
                           | otherwise = Tensor (dimensions i) (V.take l v)
                           where l = card i


instance (Bounded i, Cardinality i, MultiIndex i) =>
    FromList (Tensor i) where
        fromList = fromVector . V.fromList


instance (Bounded i, MultiIndex i) => MultiIndexable (Tensor i e) where
    type Index (Tensor i e) = i
    type Elem (Tensor i e) = e
    dims _ = maxBound
    (Tensor _ x) ! j = x V.! multiIndex2Linear j


instance (Cardinal n, MultiIndex i, MultiIndex j, MultiIndexConcat n i j)
    => DirectSummable n (Tensor i e) (Tensor j e) where
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
                  i = fromCardinal n


instance (Ordinal i, Ordinal j) => Transpose (Tensor (i :|: (j :|: Nil)) e)
    where
      type TransposeSpace (Tensor (i :|: (j :|: Nil)) e) = (Tensor (j :|: (i :|: Nil)) e)
      transpose (Tensor [d1,d2] x) = Tensor [d2,d1] (V.generate (d1*d2) t)
          where t n = let (q,r) = quotRem n d1
                      in x V.! (r * d2 + q)


unsafeTensorGet :: [Int] -> Tensor i e -> e
unsafeTensorGet is (Tensor ds x) = x V.! linearize ds is

unsafeTensorGen :: [Int] -> ([Int] -> e) -> Tensor i e
unsafeTensorGen ds f =
    Tensor ds $ V.generate (product ds) (f . (unlinearize ds))

unsafeVectorGet :: Int -> Vector i e -> e
unsafeVectorGet i (Tensor _ x) = x V.! i

unsafeVectorGen :: Int -> (Int -> e) -> Vector i e
unsafeVectorGen d f = Tensor [d] $ V.generate d f

unsafeMatrixGet :: Int -> Int -> Matrix i j e -> e
unsafeMatrixGet i j (Tensor ds x) = x V.! linearize ds [i,j]

unsafeMatrixGen :: Int -> Int -> (Int -> Int -> e) -> Matrix i j e
unsafeMatrixGen d e f = Tensor [d,e] $ V.generate (d*e)
                        (\n -> let [i,j] = unlinearize [d,e] n in
                               f i j
                        )

getMatrixEntryOnVec :: Int -- ^ Number of rows
                    -> Int -- ^ Number of columns
                    -> Int
                    -> Int
                    -> V.Vector a
                    -> a
getMatrixEntryOnVec d e i j x = x V.! linearize [d,e] [i,j]

generateMatrixOnVec :: Int -- ^ Number of rows
                    -> Int -- ^ Number of columns
                    -> (Int -> Int -> a)
                    -> V.Vector a
generateMatrixOnVec d e g = V.generate (d*e)
                            (\n -> let [i,j] = unlinearize [d,e] n in
                                   g i j
                            )

