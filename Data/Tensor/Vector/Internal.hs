{-# LANGUAGE FlexibleInstances #-}
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
            where l = product ((take i d) ++ e'')
                  e = drop i d
                  e' = drop i d'
                  e'' = ((d !! i) + (d' !! i)) : (drop (i+1) d)
                  m = product e
                  m' = product e'
                  m'' = product e''
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


unsafeMatrixRowSwitch :: Int -> Int -> Matrix i j e -> Matrix i j e
unsafeMatrixRowSwitch i1 i2 (Tensor ds x) = Tensor ds $ V.generate (d*e) rs
    where d = head ds
          e = last ds
          rs n | quot n e == i1 - 1 = x V.! (n + off)
               | quot n e == i2 - 1 = x V.! (n - off)
               | otherwise = x V.! n
          off = e * (i2 - i1)


unsafeMatrixColSwitch :: Int -> Int -> Matrix i j e -> Matrix i j e
unsafeMatrixColSwitch j1 j2 (Tensor ds x) = Tensor ds $ V.generate (d*e) cs
    where d = head ds
          e = last ds
          cs n | rem n e == j1 - 1 = x V.! (n + off)
               | rem n e == j2 - 1 = x V.! (n - off)
               | otherwise = x V.! n
          off =  j2 - j1


unsafeMatrixRowMult :: (Num e) => Int -> e -> Matrix i j e -> Matrix i j e
unsafeMatrixRowMult i a (Tensor ds x) = Tensor ds $ V.generate (d*e) rm
    where d = head ds
          e = last ds
          rm n = if quot n e == i - 1
                 then (x V.! n) * a
                 else x V.! n


unsafeMatrixColMult :: (Num e) => Int -> e -> Matrix i j e -> Matrix i j e
unsafeMatrixColMult j a (Tensor ds x) = Tensor ds $ V.generate (d*e) cm
    where d = head ds
          e = last ds
          cm n = if rem n e == j - 1
                 then (x V.! n) * a
                 else x V.! n


unsafeMatrixRowDiv :: (Fractional e) => Int -> e -> Matrix i j e -> Matrix i j e
unsafeMatrixRowDiv i a (Tensor ds x) = Tensor ds $ V.generate (d*e) rd
    where d = head ds
          e = last ds
          rd n = if quot n e == i - 1
                 then (x V.! n) / a
                 else x V.! n


unsafeMatrixColDiv :: (Fractional e) => Int -> e -> Matrix i j e -> Matrix i j e
unsafeMatrixColDiv j a (Tensor ds x) = Tensor ds $ V.generate (d*e) cd
    where d = head ds
          e = last ds
          cd n = if rem n e == j - 1
                 then (x V.! n) / a
                 else x V.! n


unsafeMatrixRowAdd :: (Num e) => Int -> e -> Int -> Matrix i j e -> Matrix i j e
unsafeMatrixRowAdd i1 a i2 (Tensor ds x) = Tensor ds $ V.generate (d*e) ra
    where d = head ds
          e = last ds
          ra n | quot n e == i1 - 1 = x V.! n + (x V.! (n + off))*a
               | otherwise = x V.! n
          off = e * (i2 - i1)


unsafeMatrixColAdd :: (Num e) => Int -> e -> Int -> Matrix i j e -> Matrix i j e
unsafeMatrixColAdd j1 a j2 (Tensor ds x) = Tensor ds $ V.generate (d*e) ca
    where d = head ds
          e = last ds
          ca n | rem n e == j1 - 1 = x V.! n + (x V.! (n + off))*a
               | otherwise = x V.! n
          off = j2 - j1


unsafeMatrixRowSub :: (Num e) => Int -> e -> Int -> Matrix i j e -> Matrix i j e
unsafeMatrixRowSub i1 a i2 (Tensor ds x) = Tensor ds $ V.generate (d*e) rs
    where d = head ds
          e = last ds
          rs n | quot n e == i1 - 1 = x V.! n - (x V.! (n + off))*a
               | otherwise = x V.! n
          off = e * (i2 - i1)


unsafeMatrixColSub :: (Num e) => Int -> e -> Int -> Matrix i j e -> Matrix i j e
unsafeMatrixColSub j1 a j2 (Tensor ds x) = Tensor ds $ V.generate (d*e) cs
    where d = head ds
          e = last ds
          cs n | rem n e == j1 - 1 = x V.! n - (x V.! (n + off))*a
               | otherwise = x V.! n
          off = j2 - j1

