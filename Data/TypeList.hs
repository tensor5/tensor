{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeList where

import Data.Cardinal
import Prelude hiding (drop, reverse)

class TypeList l where
    type Length l
    length :: l -> Length l
    length _ = undefined


class ToElem a e where
    toElem :: e -> a


class ToList a l where
    toList :: l -> [a]
    toList l = toListT l []
    toListT :: l -> [a] -> [a]
    toListT l xs = xs ++ toList l


class (TypeList l, TypeList l') => AppendList l l' where
    type l :++: l'
    (<++>) :: l -> l' -> l :++: l'


class (Cardinal n, TypeList l) => TakeList n l where
    type Take n l
    take :: n -> l -> Take n l


class (Cardinal n, TypeList l) => DropList n l where
    type Drop n l
    drop :: n -> l -> Drop n l


class (TypeList l, TypeList l') => TailRevList l l' where
    type TailRev l l'
    rev :: l -> l' -> TailRev l l'


class TypeList l => ReverseList l where
    type Reverse l
    reverse :: l -> Reverse l


class JoinList n l l' where
    type Join n l l'
    join :: n -> l -> l' -> Join n l l'

instance (ReverseList (Drop n (Reverse l)),
          DropList n (Reverse l),
          ReverseList l,
          AppendList (Reverse (Drop n (Reverse l))) (Drop n l'),
          DropList n l',
          Reverse (Take n (Reverse l)) ~ Take n l')
    => JoinList n l l' where
        type Join n l l' = (Reverse (Drop n (Reverse l))) :++: Drop n l'
        join n l l' = reverse (drop n (reverse l)) <++> drop n l'

