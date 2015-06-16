{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  © 2012-2015 Nicola Squartini
-- License     :  BSD3
--
-- Maintainer  :  Nicola Squartini <tensor5@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module define a datatype @'Tensor'@ which implements the classes and
-- methods defined in "Data.Tensor", but is represented internally as a
-- @'V.Vector'@, and is therefore faster and more memory efficient than the
-- standard @'T.Tensor'@.
--
--------------------------------------------------------------------------------

module Data.Tensor.Vector
    ( MultiIndex
    , unMultiIndex

    , Tensor
    , Vector
    , Matrix
    , ColumnVector
    , vector2ColumnVector
    , columnVector2Vector
    , RowVector

    , fromList
--    , fromVector

    , module Data.Indexable
    , module Data.MultiIndex
    , module Data.Sliceable
    , module Data.Tensor

    , Slicer

    ) where

import           Control.Applicative
import           Control.Arrow
import           Control.DeepSeq
import           Control.Exception            (throw)
import           Control.Monad                (liftM)
import           Data.Function                (on)
import           Data.Maybe                   (fromJust)
import           Data.Singletons
import           Data.Singletons.Prelude.List hiding (Reverse)
import qualified Data.Vector                  as V
import           Data.Vector.Generic          hiding (Vector, fromList,
                                               replicate, toList, (++))
import qualified Data.Vector.Generic          as G
import qualified Data.Vector.Unboxed          as U
import           Data.Word
import           GHC.Exts                     (IsList (..))
import           Prelude                      hiding (drop, head, init, length,
                                               null, product, reverse, splitAt,
                                               tail, take, zipWith)
import qualified Prelude                      as P
import           Prelude.Unicode
import           System.Random

import           Data.Indexable
import qualified Data.Indexable               as I
import           Data.MultiIndex              hiding (MultiIndex, fromList,
                                               toList)
import qualified Data.MultiIndex              as M
import           Data.Sliceable               hiding (Slicer)
import qualified Data.Sliceable               as S
import           Data.Tensor                  hiding (ColumnVector, Matrix,
                                               RowVector, Slicer (..), Tensor,
                                               Vector)


-- | An @'IsMultiIndex'@ type optimized for indexing @'Tensor'@. It is
-- internally represented as a @'U.Vector'@ of @'Word'@s.
newtype MultiIndex (is ∷ [PI]) = MultiIndex
    { unMultiIndex ∷ U.Vector Word  -- ^ Yield the internal representation of
                                    -- @'MultiIndex'@.
    } deriving Eq

-- | Standard total order on a @'MultiIndex'@ of length 1 (@[i] ≤ [j]@ iff @i ≤
-- j@). Methods have @O(1)@ complexity.
instance Ord (MultiIndex '[i]) where
    compare = compare `on` (head ∘ unMultiIndex)

instance IsMultiIndex MultiIndex where
    nil = MultiIndex G.empty
    oneCons = MultiIndex ∘ cons 1 ∘ unMultiIndex
    headSucc = MultiIndex ∘ succHead ∘ unMultiIndex
    toMultiIndex = toM sing
        where
          toM ∷ Shape is → MultiIndex is → M.MultiIndex is
          toM SNil              = const Nil
          toM (SCons SOne   sh) =
              OneCons ∘ toM sh ∘ MultiIndex ∘ tail ∘ unMultiIndex
          toM (SCons (SS i) sh) =
              headSucc ∘ toM (SCons i sh) ∘ MultiIndex ∘ predHead ∘ unMultiIndex
    toList = P.map fromIntegral ∘ toList ∘ unMultiIndex

predHead ∷ U.Vector Word → U.Vector Word
predHead = imap (\i → if i ≡ 0 then pred else id)

succHead ∷ U.Vector Word → U.Vector Word
succHead = imap (\i → if i ≡ 0 then succ else id)


-- | An @'IsTensor'@ type, internally represented as a @'V.Vector'@. It features
-- @O(r)@ @('I.!')@.
data Tensor (is ∷ [PI]) e = Tensor
    { form    ∷ U.Vector Word
    , content ∷ V.Vector e
    } deriving Eq


-- | A vector.
type Vector i = Tensor '[i]


-- | A matrix.
type Matrix i j = Tensor '[i, j]


-- | A matrix with only one column.
type ColumnVector n = Matrix n 'One

-- | Transform a vector into a one-column matrix.
vector2ColumnVector :: Vector n e -> ColumnVector n e
vector2ColumnVector (Tensor ds x) = (Tensor (ds `snoc` 1) x)

-- | Transform a one-column matrix into a vector.
columnVector2Vector :: ColumnVector n e -> Vector n e
columnVector2Vector (Tensor ds x) = (Tensor (init ds) x)


-- | A matrix with only one row.
type RowVector n = Matrix 'One n

linearize ∷ U.Vector Word -- ^ Dimension array
          → U.Vector Word -- ^ Index array
          → Int
linearize ds is = fromIntegral $ go ds is 0
    where go es js acc =
              if length js ≡ 0
              then acc
              else let t = tail es in
                   go t (tail js) (acc + (head js - 1) ⋅ product t)

unlinearize ∷ U.Vector Word -- ^ Dimension array
            → Int           -- ^ Linearized position (starting from 0)
            → U.Vector Word
unlinearize ds i = go ds (fromIntegral i) G.empty
    where go es j acc = if G.null es
                        then acc
                        else let t = tail es
                                 (q,r) = quotRem j (product t)
                             in go t r (acc `snoc` (q + 1))

unsafeTensorGet :: Tensor i e -> U.Vector Word -> e
unsafeTensorGet (Tensor ds x) = (G.!) x ∘ linearize ds

unsafeTensorGen :: U.Vector Word -> (U.Vector Word -> e) -> Tensor i e
unsafeTensorGen ds f =
    Tensor ds $ G.generate (fromIntegral $ product ds) (f ∘ (unlinearize ds))

-----------------------------------  Functor -----------------------------------

-- | @'fmap' = 'I.map'@.
instance Functor (Tensor is) where
    fmap = I.map

---------------------------------  Applicative ---------------------------------

-- | @'pure' a@ yields a @'Tensor'@ with all components equal to @a@. @'<*>'@
-- applies the @'Tensor'@ of functions componentwise.
instance SingI is ⇒ Applicative (Tensor is) where
    pure = Tensor r ∘ G.replicate (fromIntegral $ product r)
        where r = fromList $ fromShape (sing ∷ Shape is)
    (<*>) = ap

------------------------------------  Show  ------------------------------------

-- | Rank 0 @'Tensor'@s are shown as a single element, rank 1 as lists, rank 2
-- as lists of lists, and so on, using [row-major
-- order](http://en.wikipedia.org/wiki/Row-major_order).
instance Show e ⇒ Show (Tensor i e) where
    showsPrec _ (Tensor ds v) =
        let sd = reverse ds
            l = length v
            r = length ds
        in if r ≡ 0
           then shows $ v G.! 0
           else showsT (toList sd) l (replicate r 1) 1 ∘
                    (shows $ v G.! (l-1)) ∘
                    (replicate r ']' ++)
        where showsT sd l ys n =
                  let (zs,k) = match sd ys
                  in if n < l
                     then showsT sd l zs (n+1) ∘
                              (shows $  v G.! (l-n-1)) ∘
                              (replicate k ']' ++) ∘
                              (',':) ∘ (replicate k '[' ++)
                     else (replicate k '[' ++)
              match is js = match' is js [] 0
                  where match' [] _ zs n = (zs,n)
                        match' _ [] zs n = (zs,n)
                        match' (x:xs) (y:ys) zs n
                            | x ≡ y     = match' xs ys (zs ++ [1]) (n+1)
                            | otherwise = (zs ++ ((y+1):ys),n)

----------------------------------  Indexable ----------------------------------

-- | @('I.!')@ has @O(r)@ complexity.
instance Indexable Tensor where
    type Index Tensor = MultiIndex
    Tensor d u ! ix = u G.! linearize d (unMultiIndex ix)
    generate = gen sing
        where
          gen ∷ Shape is → (MultiIndex is → e) → Tensor is e
          gen s f = let r = fromList $ fromShape s
                    in Tensor r $ G.generate (fromIntegral $ product r)
                                             (f ∘ MultiIndex ∘ (unlinearize r))
    generateA = genA sing
        where
          genA ∷ Applicative f ⇒ Shape is → (MultiIndex is → f e) → f (Tensor is e)
          genA s f = let r = fromList $ fromShape s
                     in Tensor r <$> genAV (fromIntegral $ product r)
                                     (f ∘ MultiIndex ∘ (unlinearize r))
          genAV ∷ Applicative f ⇒ Int → (Int → f a) → f (V.Vector a)
          genAV n g | n ≡ 0     = pure G.empty
                    | otherwise = liftA2 cons (g 0)
                                       (genAV (n-1) (g ∘ succ))
    generateM = genM sing
        where
          genM ∷ Monad m ⇒ Shape is → (MultiIndex is → m e) → m (Tensor is e)
          genM s f = do let r = fromList $ fromShape s
                        Tensor r `liftM` G.generateM (fromIntegral $ product r)
                                         (f ∘ MultiIndex ∘ (unlinearize r))
    map f (Tensor d u) = Tensor d $ fmap f u
    ap (Tensor _ f) (Tensor d u) = Tensor d (zipWith ($) f u)

-------------------------------  MultiIndexable  -------------------------------

-- | Methods are usually implemented using @'G.generate'@ to produce the
-- underlying @'V.Vector'@.
instance MultiIndexable Tensor where
    t0 = Tensor G.empty ∘ singleton
    unT0 = head ∘ content
    concat (Tensor ds u) =
        let h = head ds
            t = tail ds
            l = fromIntegral $ product t
            j = h `cons` form (u G.! 0)
        in Tensor t $ G.generate l (\x → Tensor j
                                    $ G.concat
                                    $ [ content (u G.! (x + k⋅l)) | k ← [0..pred (fromIntegral h)] ])
    unConcat (Tensor ds u) =
        let h = head es
            l = fromIntegral (product ds ⋅ h)
        in Tensor (h `cons` ds) $ G.generate l f
        where f n = let (i, is) = quotRem n $ fromIntegral $ product ds
                    in Tensor t $ G.slice (i ⋅ tl) tl $ content (u G.! is)
              es = form (u G.! 0)
              t = tail es
              tl = fromIntegral $ product t
    Tensor ds u `at` ix =
        let h = head ds
            t = tail ds
            l = fromIntegral $ product t
            i = linearize t $ unMultiIndex ix
        in Tensor (singleton h) $
           G.generate (fromIntegral h) (\x → u G.! ((x ⋅ l) + i))
    ix `ta` Tensor ds u = let t = tail ds
                              i   = fromIntegral $ head $ unMultiIndex ix
                              l   = fromIntegral $ product t
                          in Tensor t $
                             G.slice (i ⋅ l) l u
    rev (Tensor sh v) =
        let hs = reverse sh
            zh = hs G.++ form (head v)
            s  = fromIntegral $ product sh
        in Tensor zh $ G.concat $
           P.map (content ∘ (G.!) v ∘ transposeV sh) [0 .. pred s]
    unRev (Tensor sh v) =
        Tensor zh $ G.generate (fromIntegral $ product zh) gnr
            where gnr i = let (q,r) = quotRem i s
                          in content (v G.! r) G.! transposeV rh q
                  zh = reverse rh G.++ sh
                  s = fromIntegral $ product sh
                  rh = form $ head v

-- | Index permutation yielding the @'V.Vector'@ representation of a transposed
-- tensor: (x^T)_i = x_(πi). The first argument is the dimensions array of the
-- original tensor.
transposeV ∷ U.Vector Word → Int → Int
transposeV ds = linearize ds ∘ G.reverse ∘ unlinearize (reverse ds)

----------------------------------  IsTensor  ----------------------------------

-- | @'append'@ uses @'G.generate'@ to produce the underlying @'V.Vector'@,
-- @'split'@ uses @'imap'@.
instance IsTensor Tensor where
    t1 (Tensor d u) = Tensor (1 `cons` d) u
    unT1 (Tensor d u) = Tensor (tail d) u
    (Tensor _ u) |: (Tensor d v) = let h = head d
                                       t = tail d
                                   in Tensor (succ h `cons` t) (u G.++ v)
    unCons (Tensor d u) = let t = tail d
                              (v, w) = splitAt (fromIntegral $ product t) u
                          in (Tensor t v, Tensor (pred (head d) `cons` t) w)
    append sh (Tensor d x) (Tensor e y) =
        Tensor (take i d G.++ f) (G.generate len gnr)
        where f ∷ U.Vector Word
              f = (d G.! i + e G.! i) `cons` drop (succ i) d
              len, m, n, o, i ∷ Int
              len = fromIntegral $ product (take i d G.++ f)
              m = fromIntegral $ product $ drop i d
              n = fromIntegral $ product $ drop i e
              o = fromIntegral $ product f
              i = pred $ fromPI sh
              gnr k = let (q,r) = quotRem k o
                      in if r < m
                         then x G.! (q⋅m + r)
                         else y G.! (q⋅n + r - m)
    split = split' appendSing
        where split' ∷ Append n is js ks
                     → SPI n → Tensor ks e → (Tensor is e, Tensor js e)
              split' a _ t = (u1, u2)
                  where (i, f1S) = splitDims a
                        f1, f2 ∷ U.Vector Word
                        f1 = imap (\n → if n ≡ i then const f1S else id) $
                             form t
                        f2 = imap (\n → if n ≡ i then (\x → x - f1S) else id) $
                             form t
                        u1 = unsafeTensorGen f1 (unsafeTensorGet t)
                        u2 = unsafeTensorGen f2 (unsafeTensorGet t ∘ fun)
                        fun = imap (\n → if n ≡ i then (+) f1S else id)
              splitDims ∷ Append n is js ks
                        → (Int, Word) -- ( position of split (from 0)
                                      -- , first form vector at split position)
              splitDims A1      = (0,1)
              splitDims (A1S s) = second succ $ splitDims s
              splitDims (An s)  = first succ $ splitDims s

-----------------------------------  IsList  -----------------------------------

-- | The list representation of @'Tensor'@ uses [row-major
-- order](http://en.wikipedia.org/wiki/Row-major_order).
instance SingI is ⇒ IsList (Tensor is e) where
    type Item (Tensor is e) = e
    fromList l = let s = (sing ∷ Shape is)
                     ds = fromList $ fromShape s
                 in if P.length l ≡ (fromIntegral $ product ds)
                    then Tensor ds $ fromList l
                    else throw WrongListLength
    toList = toList ∘ content

-----------------------------------  NFData  -----------------------------------

-- | Evaluate the underlying @'V.Vector'@.
instance NFData e ⇒ NFData (Tensor is e) where
    rnf (Tensor d u) = rnf d `seq` rnf u

-----------------------------------  Random  -----------------------------------

-- | Random @'Tensor'@ with independent and identically distributed components.
instance (Random e, SingI is) ⇒ Random (Tensor is e) where
    randomR (l, h) =
        let l' = toList $ content l
            h' = toList $ content h
        in first (Tensor (form l) ∘ fromList) ∘ randomListR (l', h')
        where randomListR ∷ (Random e, RandomGen g) ⇒ ([e], [e]) → g → ([e], g)
              randomListR ([]  , _   ) = (,) []
              randomListR (_   , []  ) = (,) []
              randomListR (a:as, b:bs) = (\(x, (y, z)) → (x : y, z)) ∘
                                         second (randomListR (as, bs)) ∘
                                                randomR (a, b)
    random = let s = (sing ∷ Shape is)
                 ds = fromList $ fromShape s
                 l = product ds
             in first (Tensor ds ∘ fromList) ∘ randomsWithLength l
        where randomsWithLength ∷ (Random e, RandomGen g) ⇒ Word → g → ([e], g)
              randomsWithLength 0 = (,) []
              randomsWithLength d = (\(x, (y, z)) → (x : y, z)) ∘
                                    second (randomsWithLength (d-1)) ∘ random

----------------------------------  Sliceable ----------------------------------

sliceV ∷ V.Vector a → U.Vector Word → V.Vector (Maybe Word) → V.Vector a
sliceV v sh sl = G.generate (sliceSize sh sl) (\x → v G.! f sh ks sl 0 x)
    where ks ∷ U.Vector Word
          ks = prescanr' (⋅) 1 sh
          f ∷ U.Vector Word → U.Vector Word → V.Vector (Maybe Word) → Int
            → Int → Int
          f zh js zl acc x
              | null zh   = acc
              | otherwise = case head zl of
                              Just i  → f (tail zh) (tail js) (tail zl)
                                        (acc + fromIntegral (i ⋅ head js))
                                        x
                              Nothing → let (q,r) = quotRem x
                                                    (sliceSize (tail zh)
                                                     (tail zl))
                                        in f (tail zh) (tail js) (tail zl)
                                           (acc + q ⋅ (fromIntegral $ head js))
                                           r

sliceSize ∷ U.Vector Word → V.Vector (Maybe Word) → Int
sliceSize sh = fromIntegral ∘ ifoldr' (\i m acc → case m of
                                                    Nothing → sh G.! i ⋅ acc
                                                    _       → acc
                                      ) 1

sliceSh ∷ V.Vector (Maybe Word) → U.Vector Word → U.Vector Word
sliceSh sl = ifilter (\i _ → case sl G.! i of
                               Nothing → True
                               _       → False
                     )

-- | An @'IsSlicer'@ type optimized for slicing @'Tensor'@. It is internally
-- represented as a @'V.Vector'@ of @'Maybe' 'Word'@.
newtype Slicer (i ∷ [PI]) (j ∷ [Maybe PI]) (k ∷ [PI]) = Slicer
    { unSl ∷ V.Vector (Maybe Word) }
    deriving Eq

instance IsSlicer Slicer where
    nilS = Slicer G.empty
    allCons = Slicer ∘ cons Nothing ∘ unSl
    (&) i = Slicer ∘ cons (Just $ head $ unMultiIndex $ fromMultiIndex i) ∘ unSl
    toSlicer = toSlicer' slicerShape
        where toSlicer' ∷ SlicerShape is js ks
                        → Slicer is js ks → S.Slicer is js ks
              toSlicer' NilSh          = const NilS
              toSlicer' (AllConsSh sh) =
                  AllCons ∘ toSlicer' sh ∘ Slicer ∘ tail ∘ unSl
              toSlicer' (SOne :$ ssh)  =
                  (:&) (OneCons Nil) ∘ toSlicer' ssh ∘ Slicer ∘ tail ∘ unSl
              toSlicer' (SS n :$ ssh)  =
                  bumpSl ∘ toSlicer' (n :$ ssh) ∘ Slicer ∘ predJHead ∘ unSl
              bumpSl ∷ S.Slicer (i ': is) ('Just i ': js) ks
                     → S.Slicer ('S i ': is) ('Just ('S i) ': js) ks
              bumpSl (i :& s) = HeadSucc i :& s
              predJHead ∷ V.Vector (Maybe Word) → V.Vector (Maybe Word)
              predJHead = imap (\i → if i ≡ 0
                                     then Just ∘ pred ∘ fromJust
                                     else id)

-- | Slice a @'Tensor'@ using @'G.generate'@ with an appropriate index selection
-- function on the underlying @'V.Vector'@.
instance Sliceable Tensor where
    type Sl Tensor = Slicer
    slice (Tensor sh v) (Slicer sl) = Tensor (sliceSh sl sh) (sliceV v sh sl)

--------------------------------------------------------------------------------
