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
    show (Tensor (i:[]) x) = "[" ++ showT x i
                             where showT v n | n == 1 = (show (v V.! 0)) ++ "]"
                                             | n > 1 = (show (v V.! 0))
                                                       ++ "," ++
                                                       (showT (V.tail v) (n-1))
    show (Tensor (i:is) x) = "[" ++ showT x (i*(foldl1 (*) is))
        where showT v n | n == foldl1 (*) is = (show (Tensor is v)) ++ "]"
                        | otherwise = (show (Tensor is v))
                                      ++ "," ++
                                      (showT  (V.drop (foldl1 (*) is) v) (n - (foldl1 (*) is)))

class FromVector e a | a → e where
    fromVector ∷ V.Vector e -> a

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

class TensorProduct n a b c | a → n, b → n, a b → c where
    (⊗) ∷ a → b → c

instance (Num a, HAppend i j k) ⇒ TensorProduct a (Tensor i a) (Tensor j a) (Tensor k a)
    where
      (Tensor d x) ⊗ (Tensor e y) =
          Tensor (d ++ e) (
                           V.generate ((V.length x) * (V.length y)) genP
                          )
              where genP = \n → (x V.! (n `div` (V.length y))) *
                           (y V.! (n `mod` (V.length y)))

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


class TensorProduct' n a b c | a → n, b → n, a b → c where
    tProd ∷ a → b → c

instance (Num a, Product a End t1 t2 t3) ⇒ TensorProduct' a t1 t2 t3 where
    tProd = prod End

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