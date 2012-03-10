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
import           Data.Tensor hiding (Tensor)
import qualified Data.Tensor as T
import qualified Data.Vector as V

data Tensor i e = Tensor
    { form :: [Int]
    , content :: V.Vector e
    } deriving Eq


type ColumnVector n = Tensor (n :|: Nil)


type Vector n = ColumnVector n


type RowVector n = Tensor (One :|: (n :|: Nil))


type Matrix m n = Tensor (m :|: (n :|: Nil))


instance Show e => Show (Tensor i e) where
    showsPrec _ (Tensor [] v) = shows $ v V.! 0
    showsPrec _ (Tensor ds v) = let sd = Prelude.reverse ds in
                                let l = V.length v in
                                let r = Prelude.length ds in
                                showsT sd l (Prelude.replicate r 1) 1 .
                                     (shows $ v V.! (l-1)) .
                                     (Prelude.replicate r ']' ++)
        where showsT sd l ys n = let (zs,k) = match sd ys in
                               if n < l
                               then showsT sd l zs (n+1) .
                                     (shows $  v V.! (l-n-1)) .
                                        (Prelude.replicate k ']' ++) .
                                        (',':) . (Prelude.replicate k '[' ++)
                               else (Prelude.replicate k '[' ++)
              match is js = match' is js [] 0
                  where match' [] _ zs n = (zs,n)
                        match' _ [] zs n = (zs,n)
                        match' (x:xs) (y:ys) zs n | x == y =
                                                      match' xs ys (zs ++ [1]) (n+1)
                                                  | otherwise =
                                                      (zs ++ ((y+1):ys),n)


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


instance (Bounded i, MultiIndex i) => T.Tensor (Tensor i e) where
    type Index (Tensor i e) = i
    type Elem (Tensor i e) = e
    dims _ = maxBound
    (Tensor _ x) ! j = x V.! multiIndex2Linear j
    generate f = t
        where t = Tensor d $
                  V.generate (product d) (f . toMultiIndex . (unlinearize d))
              d = fromMultiIndex $ dims $ asTypeOf undefined t
    replicate e = t
        where t = Tensor d $
                  V.replicate (product d) e
              d = fromMultiIndex $ dims $ asTypeOf undefined t


instance (Cardinal n, MultiIndex i, MultiIndex j, MultiIndexConcat n i j)
    => DirectSum n (Tensor i e) (Tensor j e) where
        type SumSpace n (Tensor i e) (Tensor j e) = (Tensor (Concat n i j) e)
        directSum n (Tensor d x) (Tensor d' y) = Tensor ((take i d) ++ e'') (V.generate l g)
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
      transpose (Tensor ds x) = Tensor [d2,d1] (V.generate (d1*d2) t)
          where t n = let (q,r) = quotRem n d1
                      in x V.! (r * d2 + q)
                d1 = head ds
                d2 = head $ tail ds


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

unsafeMatrixGetRow :: Int -> Matrix i j e -> Vector j e
unsafeMatrixGetRow i (Tensor ds x) = Tensor (tail ds) $
                                     V.slice ((i-1)*d) d x
    where d = last ds
