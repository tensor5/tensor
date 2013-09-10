{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tensor.Pure
    ( Tensor(..)
    , Vector
    , Matrix

    , module Data.Tensor
    , module Data.TypeList.MultiIndex

    ) where

import           Control.Applicative
import           Data.Tensor hiding (Tensor)
import qualified Data.Tensor as T (Tensor)
import           Data.TypeList.MultiIndex hiding ((<*>))


data family Tensor is e


data instance Tensor Nil e = T0 e
                             deriving Eq

instance Dimensions (Tensor Nil e) where
    dimensions _ = []

instance FromList (Tensor Nil) where
    fromList [] = error "Data.Tensor.fromList: length of list must be at \
                        \least 1"
    fromList (e:_) = T0 e

instance Functor (Tensor Nil) where
    fmap f (T0 e) = T0 $ f e

instance Applicative (Tensor Nil) where
    pure e = T0 e
    (T0 f) <*> T0 e = T0 (f e)

instance Show e => Show (Tensor Nil e) where
    show (T0 e) = show e

instance T.Tensor (Tensor Nil e) where
    type Index (Tensor Nil e) = Nil
    type Elem (Tensor Nil e) = e
    T0 e ! _ = e
    generate f = T0 $ f Nil
    generateM f = f Nil >>= return . T0


data instance MultiIndex is => Tensor (One :|: is) e = T1 (Tensor is e)

instance (MultiIndex is, Eq (Tensor is e)) => Eq (Tensor (One :|: is) e) where
    (T1 t) == (T1 u) = t == u

instance Dimensions (Tensor is e) => Dimensions (Tensor (One :|: is) e) where
    dimensions t = 1 : dimensions (f t)
        where f :: Tensor (One :|: is) e -> Tensor is e
              f _ = undefined

instance ( FromList (Tensor is)
         , MultiIndex is
         ) => FromList (Tensor (One :|: is)) where
    fromList = T1 . fromList

instance ( MultiIndex is
         , Functor (Tensor is)
         ) => Functor (Tensor (One :|: is)) where
    fmap f (T1 t) = T1 $ fmap f t

instance ( MultiIndex is
         , Applicative (Tensor is)
         ) => Applicative (Tensor (One :|: is)) where
    pure e = T1 (pure e)
    (T1 f) <*> T1 t = T1 (f <*> t)

instance ( MultiIndex is
         , Show (Tensor is e)
         ) => Show (Tensor (One :|: is) e) where
    showsPrec n (T1 t) = ('[':) . showsPrec n t . (']':)

instance ( MultiIndex is
         , T.Tensor (Tensor is e)
         , e ~ Elem (Tensor is e)
         , is ~ Index (Tensor is e)
         ) => T.Tensor (Tensor (One :|: is) e) where
    type Index (Tensor (One :|: is) e) = One :|: is
    type Elem (Tensor (One :|: is) e) = e
    T1 t ! (_ :|: is) = t ! is
    generate f = T1 $ generate (\is -> f (undefined :|: is))
    generateM f = generateM (\is -> f (undefined :|: is)) >>= return . T1


data instance (Ordinal n, MultiIndex is)
    => Tensor (Succ n :|: is) e = (Tensor is e) :!: (Tensor (n :|: is) e)

infixr 9 :!:

instance ( MultiIndex is
         , Ordinal n
         , Eq (Tensor is e)
         , Eq (Tensor (n :|: is) e)
         ) => Eq (Tensor (Succ n :|: is) e) where
    (t :!: ts) == (u :!: us) = t == u && ts == us

instance Dimensions (Tensor (n :|: is) e) =>
    Dimensions (Tensor (Succ n :|: is) e) where
        dimensions t = let (d:ds) = dimensions (f t)
                       in d+1:ds
            where f :: Tensor (Succ n :|: is) e -> Tensor (n :|: is) e
                  f _ = undefined

instance ( FromList (Tensor is)
         , FromList (Tensor (n :|: is))
         , Ordinal n
         , MultiIndex is
         ) => FromList (Tensor (Succ n :|: is)) where
    fromList es | Prelude.length es >= product d =
                    t $ Prelude.take (product d) es
                | otherwise = error ("Data.Tensor.fromList: length of list \
                                     \must be at least " ++ show (product d))
        where t x = let (es1,es2) = splitAt (product $ Prelude.tail d) x
                    in fromList es1 :!: fromList es2
              d = dimensions $ shape $ t undefined
              shape :: Tensor (Succ n :|: is) e -> Succ n :|: is
              shape _ = undefined

instance ( MultiIndex is
         , Ordinal n
         , Functor (Tensor is)
         , Functor (Tensor (n :|: is))
         ) => Functor (Tensor (Succ n :|: is)) where
    fmap f (t :!: ts) = fmap f t :!: fmap f ts

instance ( MultiIndex is
         , Ordinal n
         , Applicative (Tensor is)
         , Applicative (Tensor (n :|: is))
         ) => Applicative (Tensor (Succ n :|: is)) where
    pure e = pure e :!: pure e
    (f :!: fs) <*> (t :!: ts) = (f <*> t) :!: (fs <*> ts)

instance ( MultiIndex is
         , Ordinal n
         , Show (Tensor is e)
         , Show (Tensor (n :|: is) e)
         ) => Show (Tensor (Succ n :|: is) e) where
    showsPrec n (t :!: ts) = ('[':) .
                             (showsPrec n t) .
                             (',':) .
                             Prelude.tail .
                             showsPrec n ts

instance ( MultiIndex is
         , Ordinal n
         , T.Tensor (Tensor is e)
         , T.Tensor (Tensor (n :|: is) e)
         , e ~ Elem (Tensor is e)
         , is ~ Index (Tensor is e)
         , e ~ Elem (Tensor (n :|: is) e)
         , Index (Tensor (n :|: is) e) ~ (n :|: is)
         ) => T.Tensor (Tensor (Succ n :|: is) e) where
    type Index (Tensor (Succ n :|: is) e) = Succ n :|: is
    type Elem (Tensor (Succ n :|: is) e) = e
    (t :!: _) ! (First :|: is) = t ! is
    (_ :!: ts) ! (Succ n :|: is) = ts ! (n :|: is)
    generate f = (generate (\is -> f (First :|: is)) :!:
                      generate (\(n :|: is) -> f (Succ n :|: is)))
    generateM f = do
      a <- generateM (\is -> f (First :|: is))
      b <- generateM (\(n :|: is) -> f (Succ n :|: is))
      return $ (a :!: b)


type Vector i = Tensor (i :|: Nil)

type Matrix i j = Tensor (i :|: (j :|: Nil))

