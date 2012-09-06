{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tensor.Vector.Internal where

import           Data.Cardinal
import           Data.TypeList.MultiIndex hiding ((!!), head, drop, length, tail, take)
import           Data.TypeList.MultiIndex.Internal
import           Data.Ordinal
import           Data.Tensor hiding (Tensor)
import qualified Data.Tensor as T
import qualified Data.Vector as V
import           Data.Tensor.LinearAlgebra hiding (Matrix)
import qualified Data.Tensor.LinearAlgebra as LA


data Tensor i e = Tensor
    { form :: [Int]
    , content :: V.Vector e
    } deriving Eq


type Vector n = Tensor (n :|: Nil)


type Matrix m n = Tensor (m :|: (n :|: Nil))


type ColumnVector n = Matrix n One

vector2ColumnVector :: Vector n e -> ColumnVector n e
vector2ColumnVector (Tensor ds x) = (Tensor (ds ++ [1]) x)

columnVector2Vector :: ColumnVector n e -> Vector n e
columnVector2Vector (Tensor ds x) = (Tensor (init ds) x)


type RowVector n = Matrix One n

vector2RowVector :: Vector n e -> RowVector n e
vector2RowVector (Tensor ds x) = (Tensor (1 : ds) x)

rowVector2Vector :: RowVector n e -> Vector n e
rowVector2Vector (Tensor ds x) = (Tensor (tail ds) x)

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


instance MultiIndex i => FromVector (Tensor i) where
    fromVector x = toTensor undefined x
        where
          toTensor :: MultiIndex i => i -> V.Vector e -> Tensor i e
          toTensor i v | V.length v < l = error ("fromVector: length of vector \
                                                 \must be at least " ++ show l)
                       | otherwise = Tensor (dimensions i) (V.take l v)
              where l = product $ dimensions i


instance MultiIndex i => FromList (Tensor i) where
    fromList = fromVector . V.fromList


instance MultiIndex i => T.Tensor (Tensor i e) where
    type Index (Tensor i e) = i
    type Elem (Tensor i e) = e
    (Tensor _ x) ! j = x V.! multiIndex2Linear j
    generate f = t
        where t = Tensor d $
                  V.generate (product d) (f . toMultiIndex . (unlinearize d))
              d = dimensions t


instance Dimensions i => Dimensions (Tensor i e) where
    dimensions t = dimensions $ shape t
                    where shape :: Tensor i e -> i
                          shape _ = undefined


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
        split n t = (t1,t2)
            where t1 = unsafeTensorGen d1 (\j -> unsafeTensorGet j t)
                  d1 = dimensions t1
                  t2 = unsafeTensorGen d2 (\j -> unsafeTensorGet (f j) t)
                  d2 = dimensions t2
                  i = fromCardinal n
                  d1i = d1 !! i
                  f j = if i == 0
                        then ((j!!0) + d1i) : (drop 1 j)
                        else (take i j) ++ (((j!!i) + d1i) : (drop (i + 1) j))


instance (Ordinal i, Ordinal j) => Transpose (Tensor (i :|: (j :|: Nil)) e)
    where
      type TransposeSpace (Tensor (i :|: (j :|: Nil)) e) = (Tensor (j :|: (i :|: Nil)) e)
      transpose (Tensor ds x) = Tensor [d2,d1] (V.generate (d1*d2) t)
          where t n = let (q,r) = quotRem n d1
                      in x V.! (r * d2 + q)
                d1 = head ds
                d2 = head $ tail ds


instance (MultiIndex i,
          MultiIndex j,
          Extend i l,
          ReverseList j,
          ReverseList (Ext i l),
          Extend (Reverse j) (Reverse (Ext i l)),
          ReverseList (Ext (Reverse j) (Reverse (Ext i l)))
         ) => Sliceable i j (Tensor l e) where
    type Slice i j (Tensor l e) =
        Tensor (Reverse (Ext (Reverse j) (Reverse (Ext i l)))) e
    slice i j t = unsafeTensorGen f (\k ->  unsafeTensorGet (ii ++ k ++ jj) t)
        where ii = fromMultiIndex i
              jj = fromMultiIndex j
              f = drop (Prelude.length ii) $
                  take ((Prelude.length $ form t) - Prelude.length jj) (form t)


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


instance MultiIndex i => VectorSpace (Tensor i) where
    zero = T.replicate 0
    a *. t = fmap (* a) t
    (.+.) = T.zipWith (+)


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


instance DotProduct (Tensor i) where
    dot (Tensor _ x) (Tensor _ y) = V.sum $ V.zipWith (*) x y


instance (Ordinal i, Ordinal j) => LA.Matrix i j (Tensor (i :|: (j :|: Nil)) e)
    where
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
      rowAdd i1 a i2 m =
          unsafeMatrixRowAdd (fromOrdinal i1) a (fromOrdinal i2) m
      colAdd j1 a j2 m =
          unsafeMatrixColAdd (fromOrdinal j1) a (fromOrdinal j2) m
      rowEchelonForm m = let (Tensor [_,e] _) = m in
                         fst $ partialRowEchelon m e


instance (Ordinal i, Sum i i) => SquareMatrix (Tensor (i :|: (i :|: Nil))) where
    unit = u
           where u = Tensor d $ V.generate (i*i) g
                 g n = if rem n (i + 1) == 0
                       then 1
                       else 0
                 i = head d
                 d = dimensions u
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
    minPoly x = let (y,n) = gaussBigMatr x in
                go y n n []
        where go z r k ls = if k == 0
                            then ls
                            else go z r (k-1) ((negate (unsafeMatrixGet (r+1) k z)):ls)
    polyEval _ [] = zero
    polyEval _ [e] = e *. unit
    polyEval x (e:es) = (e *. unit) .+. (x .*. (polyEval x es))


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


instance (Eq e, Fractional e, Ordinal i, Ordinal j, Ordinal k, Sum j k) =>
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


instance (Eq e, Fractional e, Ordinal i, Ordinal j) =>
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


-- |Returns the position of the first element satisfying the
-- predicate, in the row i, starting from column j.
findInRow :: Int -> Int -> (e -> Bool) -> Matrix i j e -> Maybe Int
findInRow i j p x = go (last $ form x) j
    where go d k | k <= d = if p $ unsafeMatrixGet i k x
                            then Just k
                            else go d (k+1)
                 | otherwise = Nothing


-- |Returns the position of the first element satisfying the
-- predicate, in the column j, starting from row i.
findInCol :: Int -> Int -> (e -> Bool) -> Matrix i j e -> Maybe Int
findInCol j i p x = go (head $ form x) i
    where go d k | k <= d = if p $ unsafeMatrixGet k j x
                            then Just k
                            else go d (k+1)
                 | otherwise = Nothing


-- | '@rowEchelonOnAugmented@ a b' runs elementary row operation on
-- the augmented matrix '[a|b]' until 'a' is in reduced row echelon
-- form. The result is ((c,d),n), where [c|d] is the resulting matrix,
-- and n is the rank of a.
rowEchelonOnAugmented :: (Eq e, Fractional e, Sum j k, Ordinal i, Ordinal j, Ordinal k) =>
                         Matrix i j e
                      -> Matrix i k e
                      -> ((Matrix i j e, Matrix i k e), Int)
rowEchelonOnAugmented m1 m2 = let m = directSum (undefined :: C1) m1 m2
                                  (a, n) = partialRowEchelon m (last $ form m1)
                              in (split (undefined :: C1) a, n)


partialRowEchelon :: (Eq e, Fractional e) =>
                     Matrix i j e
                  -> Int
                  -> (Matrix i j e, Int)
partialRowEchelon m e = let (Tensor [d,_] _) = m in
                        rEch m d 1 1 0
    where rEch x d i j n = if i > d || j > e
                              then (x, n)
                              else case gaussSwitchToNonZeroRow i j x of
                                     Just y -> let y' = (unsafeMatrixRowDiv i
                                                         (unsafeMatrixGet i j x)
                                                         y) in
                                               rEch (gaussClearColUp i j $ gaussClearColDown i j y') d (i+1) (j+1) (n+1)
                                     Nothing -> rEch x d i (j+1) n


-- |Find the firt non-zero element in the part of the column j
-- starting from the element (i,j) going down, and switch the row with
-- it.
gaussSwitchToNonZeroRow :: (Eq e, Num e) =>
                           Int -> Int -> Matrix i j e -> Maybe (Matrix i j e)
gaussSwitchToNonZeroRow i j m = case findInCol j i (/= 0) m of
                               Just k -> Just $ unsafeMatrixRowSwitch i k m
                               Nothing -> Nothing


-- |Find the firt non-zero element in the part of the row i starting
-- from the element (i,j) going right, and switch the column with it.
gaussSwitchToNonZeroCol :: (Eq e, Num e) =>
                           Int -> Int -> Matrix i j e -> Maybe (Matrix i j e)
gaussSwitchToNonZeroCol i j m = case findInRow i j (/= 0) m of
                                  Just k -> Just $ unsafeMatrixColSwitch j k m
                                  Nothing -> Nothing


-- |By adding multiples of rows to each other, makes zero all the
-- elements above (i,j).
gaussClearColUp :: (Eq e, Num e) => Int -> Int -> Matrix i j e -> Matrix i j e
gaussClearColUp i j m = go m (i-1) (unsafeMatrixGet i j m)
    where go x i' a = if i' == 0
                      then x
                      else if a == 1
                           then go (unsafeMatrixRowSub i' (unsafeMatrixGet i' j x) i x) (i'-1) a
                           else go (unsafeMatrixRowSub i' (unsafeMatrixGet i' j x) i (unsafeMatrixRowMult i' a x)) (i'-1) a


-- |By adding multiples of rows to each other, makes zero all the
-- elements below (i,j).
gaussClearColDown :: (Eq e, Num e) => Int -> Int -> Matrix i j e -> Matrix i j e
gaussClearColDown i j m = let (Tensor [d,_] _) = m in
                          go m d (i+1) (unsafeMatrixGet i j m)
    where go x d i' a = if i' > d
                        then x
                        else if a == 1
                             then go (unsafeMatrixRowSub i' (unsafeMatrixGet i' j x) i x) d (i'+1) a
                           else go (unsafeMatrixRowSub i' (unsafeMatrixGet i' j x) i (unsafeMatrixRowMult i' a x)) d (i'+1) a


-- |By adding multiples of columns to each other, makes zero all the
-- elements to the left of (i,j).
gaussClearRowLeft :: (Eq e, Num e) => Int -> Int -> Matrix i j e -> Matrix i j e
gaussClearRowLeft i j m = go m (j-1) (unsafeMatrixGet i j m)
    where go x j' a = if j' == 0
                      then x
                      else if a == 1
                           then go (unsafeMatrixColSub j' (unsafeMatrixGet i j' x) j x) (j'-1) a
                           else go (unsafeMatrixColSub j' (unsafeMatrixGet i j' x) j (unsafeMatrixColMult j' a x)) (j'-1) a


-- |By adding multiples of columns to each other, makes zero all the
-- elements to the right of (i,j).
gaussClearRowRight :: (Eq e, Num e) =>
                      Int -> Int -> Matrix i j e -> Matrix i j e
gaussClearRowRight i j m = let (Tensor [_,d] _) = m in
                           go m d (j+1) (unsafeMatrixGet i j m)
    where go x d j' a = if j' > d
                        then x
                        else if a == 1
                             then go (unsafeMatrixColSub j' (unsafeMatrixGet i j' x) j x) d (j'+1) a
                             else go (unsafeMatrixColSub j' (unsafeMatrixGet i j' x) j (unsafeMatrixColMult j' a x)) d (j'+1) a


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


bigMatr :: (Num e, Sum i i, Ordinal i)
           => Matrix i i e -> Matrix (Data.Ordinal.Succ i) (i :*: i) e
bigMatr x = Tensor [n+1,n^(2::Int)] $
            V.concat (map content (take (n+1) $ iterate (.*. x) (asTypeOf unit x)))
    where n = head $ dimensions x


gaussBigMatr :: (Eq e, Fractional e, Sum i i, Ordinal i) =>
                Matrix i i e -> (Matrix (Data.Ordinal.Succ i) (i :*: i) e, Int)
gaussBigMatr m = let l = bigMatr m
                     (Tensor [_,d] _) = l in
                 cEch l d 1 1 0
    where cEch x d i j n = if j > d
                           then (x,n)
                           else case gaussSwitchToNonZeroCol i j x of
                                  Just y -> let y' = (unsafeMatrixColDiv j
                                                      (unsafeMatrixGet i j x)
                                                      y) in
                                            cEch (gaussClearRowLeft i j $ gaussClearRowRight i j y') d (i+1) (j+1) (n+1)
                                  Nothing -> (x,n)

