{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The Module "Data.TypeList" is a collection of classes to
-- manipulate lists of types, a.k.a. heterogeneous lists.  Check the
-- module "Data.TypeList.MultiIndex" for a concrete implementation
-- of @'TypeList'@.

module Data.TypeList where

import Data.Cardinal
import Prelude hiding (drop, reverse)


-- | Every @'TypeList'@ has a @'Length'@. The @'Length'@ is actually a
-- type, and should be a @'Cardinal'@ (see "Data.Cardinal").
class TypeList l where
    type Length l
    length :: l -> Length l
    length _ = undefined


-- | A class for appending two @'TypeList'@s. The result of appending
-- @l@ and @l'@ has type @l ':++:' l'@.
class (TypeList l, TypeList l') => AppendList l l' where
    type l :++: l'
    (<++>) :: l -> l' -> l :++: l'


-- | This is does for @'TakeList'@ what @'Prelude.take'@ does for
-- ordinary lists.
class (Cardinal n, TypeList l) => TakeList n l where
    type Take n l
    take :: n -> l -> Take n l


-- | This is does for @'TakeList'@ what @'Prelude.drop'@ does for
-- ordinary lists.
class (Cardinal n, TypeList l) => DropList n l where
    type Drop n l
    drop :: n -> l -> Drop n l


-- | Reverse @l@ and append it in front of @l'@.
class (TypeList l, TypeList l') => TailRevList l l' where
    type TailRev l l'
    rev :: l -> l' -> TailRev l l'


-- | Reverse the @'TypeList'@ @l@, and get @'Reverse' l@.
class TypeList l => ReverseList l where
    type Reverse l
    reverse :: l -> Reverse l


-- | Join together @'TypeList'@s @l@ and @l'@ where the last @n@ types
-- of @l@ coincide with the first @n@ types of @l'@. The result has a
-- the common @n@ types eliminated.
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

