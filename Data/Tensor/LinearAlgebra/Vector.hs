{-# LANGUAGE FlexibleInstances #-}
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
import           Data.TypeList.MultiIndex hiding (head, take, drop, length)
import           Data.Ordinal
import           Data.Tensor.LinearAlgebra hiding (Matrix)
import qualified Data.Tensor.LinearAlgebra as LA
import           Data.Tensor.Vector
import           Data.Tensor.Vector.Internal
import qualified Data.Vector as V
import           Prelude hiding (replicate, zipWith)
import qualified Prelude as P

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


instance (Bounded i, Ordinal i, Bounded j, Ordinal j) => LA.Matrix i j (Tensor (i :|: (j :|: Nil)) e) where
    rowSwitch i1 i2 m | i1 /= i2 = unsafeMatrixRowSwitch
                                   (fromOrdinal i1)
                                   (fromOrdinal i2)
                                   m
                      | otherwise = m
    colSwitch j1 j2 m | j1 /= j2 = unsafeMatrixColSwitch
                                   (fromOrdinal j1)
                                   (fromOrdinal j2)
                                   m
                      | otherwise = m
    rowMult i a m = unsafeMatrixRowMult (fromOrdinal i) a m
    colMult j a m = unsafeMatrixColMult (fromOrdinal j) a m
    rowAdd i1 a i2 m = unsafeMatrixRowAdd (fromOrdinal i1) a (fromOrdinal i2) m
    colAdd j1 a j2 m = unsafeMatrixColAdd (fromOrdinal j1) a (fromOrdinal j2) m
    rowEchelonForm m = let (Tensor [_,e] _) = m in
                       fst $ partialRowEchelon m e


instance (Bounded i, Ordinal i, Sum i i) =>  SquareMatrix (Tensor (i :|: (i :|: Nil))) where
    unit = u
           where u = Tensor d $ V.generate (i*i) g
                 g n = if rem n (i + 1) == 0
                       then 1
                       else 0
                 i = head d
                 d = dimensions $ dims (asTypeOf undefined u)
    inverse m = let (f,s) = triangularSolve m u in
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


traceOnVec :: (Num a) =>
              Int -- ^ Number of rows (or columns)
           -> V.Vector a -- ^ Vector representation of the matrix
           -> a
traceOnVec d x = trace 0 0
    where trace i acc = if i < d*d
                        then trace (i + d + 1) (acc + (x V.! i))
                        else acc


initClow :: Num e =>
            Matrix i i e -- ^ Input matrix
         -> Matrix i i e -- ^ Matrix of length 2 clows
initClow x = unsafeMatrixGen d d g
    where g c0 c' | c0 < c' = a c0 c'
                  | c0 == c' = negate $ sum [ a c'' c'' | c'' <- [1 .. c'-1]]
                  | otherwise = 0
          a i j = unsafeMatrixGet i j x
          d = head $ form x


-- | Makes one more step in the clow sequence
clowStep :: Num e =>
            Matrix i i e -- ^ Input matrix
         -> Matrix i i e -- ^ Input clow nodes of length l
         -> Matrix i i e -- ^ Output clow nodes of length l+1
clowStep x y = unsafeMatrixGen d d g
    where g c0 c' | c0 < c' = sum [(b c0 c)*(a c c') | c <- [c0 .. d]]
                  | c0 == c' = negate $ sum [(b c''  c)*(a c c'') | c'' <- [1 .. c'-1], c <- [c'' .. d]]
                  | otherwise = 0
          a i j = unsafeMatrixGet i j x
          b i j = unsafeMatrixGet i j y
          d = head $ form x


endClow :: Num e =>
           Matrix i i e -- ^ Input matrix
        -> Matrix i i e -- ^ Input clow nodes of length l
        -> e
endClow x y = negate $ sum [(b c''  c)*(a c c'') | c'' <- [1 .. d], c <- [c'' .. d]]
    where a i j = unsafeMatrixGet i j x
          b i j = unsafeMatrixGet i j y
          d = head $ form x


instance (Eq e, Fractional e, Bounded i, Bounded j, Bounded k, Ordinal i, Ordinal j, Ordinal k, Sum j k) =>
    LinearSystem (Tensor (i :|: (j :|: Nil)) e) (Tensor (i :|: (k :|: Nil)) e)
        where
          type SolSpace (Tensor (i :|: (j :|: Nil)) e) (Tensor (i :|: (k :|: Nil)) e) = (Tensor (j :|: (k :|: Nil)) e)
          triangularSolve m1 m2 =
              let ((a, b), _) = rowEchelonOnAugmented m1 m2 in
              (a, b)
          parametricSolve m1 m2 =
              let ((t1, t2), n) = rowEchelonOnAugmented m1 m2 in
              pSolve t1 t2 n
                 where pSolve t1 t2 n
                           | solExists (n + 1) =
                               let (v,vs) = constructSol n 0 (firstNonZeroInRow n t1) (V.empty,[]) in
                               Just (Tensor [d,e] v, map ((Tensor [d,e]) . (repl e)) vs)
                           | otherwise = Nothing
                        where d = last $ form t1
                              e = last $ form t2
                              repl m v = V.generate (m * V.length v) (\x -> v V.! quot x m)
                              -- constructSol :: Int  -- ^Current row
                              --              -> Int  -- ^Position of leading one in previos row, starting from the end of the row
                              --              -> Int  -- ^Position of leading one in current row
                              --              -> (V.Vector e,[V.Vector e])
                              --              -> (V.Vector e,[V.Vector e])
                              constructSol m k f (v,vs)
                                  | m == 0 = addFreeVars k e (d-k) (v,vs)
                                  | otherwise = constructSol (m - 1) (d - f + 1) (firstNonZeroInRow (m - 1) t1)
                                                ((content (unsafeMatrixGetRow m t2)) V.++ (addFreeVarsSol e (d - f - k) v), fr)
                                  where
                                    fr = addEntryKer (V.slice f (d - f) (content (unsafeMatrixGetRow m t1))) $ addFreeVarsKer k (d - f - k) vs
                              -- returns False if the system has no
                              -- solution, otherwise True. Notice: the
                              -- augmented matrix [x|y] should be in
                              -- reduced row echelon form.
                              solExists i | i <= n = if isZeroRow i t2
                                                     then solExists (i+1)
                                                     else False
                                          |  otherwise = True


instance (Eq e, Fractional e, Bounded i, Bounded j, Ordinal i, Ordinal j) =>
    LinearSystem (Tensor (i :|: (j :|: Nil)) e) (Tensor (i :|: Nil) e)
        where
          type SolSpace (Tensor (i :|: (j :|: Nil)) e) (Tensor (i :|: Nil) e) = (Tensor (j :|: Nil) e)
          triangularSolve a b =
              let (a', b') = triangularSolve a (vector2ColumnVector b) in
              (a', columnVector2Vector b')
          parametricSolve a b =
              let s = parametricSolve a (vector2ColumnVector b) in
              case s of
                Just (v, vs) ->
                    Just (columnVector2Vector v, map columnVector2Vector vs)
                Nothing -> Nothing


-- |Checks if i-th row of x is zero
isZeroRow :: (Eq e, Num e) => Int -> Matrix i j e -> Bool
isZeroRow i x = isZero (last $ form x) 1
    where isZero d k | k <= d = if unsafeMatrixGet i k x == 0
                                then isZero d (k+1)
                                else False
                     | otherwise = True


-- |Returns the position of the first non-zero element in the n-th row
-- of a matrix, or zero if the row is made of all zeroes
firstNonZeroInRow :: (Eq e, Num e) => Int -> Matrix i j e -> Int
firstNonZeroInRow n x = go (last $ form x) 1
    where go d k | k <= d = if unsafeMatrixGet n k x /= 0
                            then k
                            else go d (k+1)
                 | otherwise = 0


-- | '@rowEchelonOnAugmented@ a b' runs elementary row operation on
-- the augmented matrix '[a|b]' until 'a' is in reduced row echelon
-- form. The result is ((c,d),n), where [c|d] is the resulting matrix,
-- and n is the rank of a.
rowEchelonOnAugmented :: (Eq e, Fractional e, Bounded i, Bounded j, Bounded k, Sum j k, Ordinal i, Ordinal j, Ordinal k) =>
                         Matrix i j e
                      -> Matrix i k e
                      -> ((Matrix i j e, Matrix i k e), Int)
rowEchelonOnAugmented m1 m2 = let m = directSum (undefined :: C1) m1 m2
                                  (a, n) = partialRowEchelon m (last $ form m1)
                              in (proj (undefined :: C1) a, n)


partialRowEchelon :: (Eq e, Fractional e) =>
                     Matrix i j e
                  -> Int
                  -> (Matrix i j e, Int)
partialRowEchelon m e = let (Tensor [d,_] _) = m in
                        rEch m d 1 1 2 0
    where rEch x d i j i' n = if i > d || j > e
                              then (x, n)
                              else if unsafeMatrixGet i j x == 0
                                   then if i' <= d
                                        then rEch
                                                 (unsafeMatrixRowSwitch i i' x)
                                                 d i j (i'+1) n
                                        else rEch x d i (j+1) (i+1) n
                                   else rEch
                                            (rEl (unsafeMatrixRowDiv i
                                                  (unsafeMatrixGet i j x)
                                                  x)
                                             d i j 1)
                                            d (i+1) (j+1) (i+2) (n+1)
          -- Assuming the (i,j) element of the matrix is 1, makes 0
          -- all the other elements in the j-th column
          rEl x d i j i' = if i' > d
                           then x
                           else if i' == i
                                then rEl x d i j (i'+1)
                                else rEl
                           (unsafeMatrixRowSub i' (unsafeMatrixGet i' j x) i x)
                           d i j (i'+1)


addFreeVarsKer :: (Num e) => Int -> Int -> [V.Vector e] -> [V.Vector e]
addFreeVarsKer _ 0 vs = vs
addFreeVarsKer k n vs = addFree ++ (map ((V.++) (V.replicate n 0)) vs)
    where addFree = map genFree (enumFromTo 1 n)
          genFree i = (V.replicate (i-1) 0) V.++ (V.cons 1 $ V.replicate (n-i+k) 0)


addEntryKer :: (Num e) => V.Vector e -> [V.Vector e] -> [V.Vector e]
addEntryKer v vs = map addE vs
    where addE x = V.cons (negate $ V.sum $ V.zipWith (*) v x) x


addFreeVarsSol :: (Num e) => Int -> Int -> V.Vector e -> V.Vector e
addFreeVarsSol _ 0 v = v
addFreeVarsSol d n v = (V.replicate (n*d) 0) V.++ v


addFreeVars :: (Num e) =>
               Int
            -> Int
            -> Int
            -> (V.Vector e,[V.Vector e])
            -> (V.Vector e,[V.Vector e])
addFreeVars k d n (v,vs) = (addFreeVarsSol d n v, addFreeVarsKer k n vs)


