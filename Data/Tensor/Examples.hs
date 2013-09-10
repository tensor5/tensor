{-|

This library defines data types and classes for fixed dimension
vectors and tensors. The main objects are:

[@'Data.Ordinal.Ordinal'@] A totally ordered set with fixed size. The
@'Data.Ordinal.Ordinal'@ type @'Data.Ordinal.One'@ contains 1 element,
@'Data.Ordinal.Succ' 'Data.Ordinal.One'@ contains 2 elements,
@'Data.Ordinal.Succ' 'Data.Ordinal.Succ' 'Data.Ordinal.One'@ contains
3 elements, and so on (see "Data.Ordinal" for more details). The type
@'Data.Ordinal.Two'@ is an alias for @'Data.Ordinal.Succ'
'Data.Ordinal.One'@, @'Data.Ordinal.Three'@ is an alias for
@'Data.Ordinal.Succ' 'Data.Ordinal.Succ' 'Data.Ordinal.One'@, and so
on.

[@'Data.TypeList.MultiIndex.MultiIndex'@] The index set. It can be
linear, rectangular, parallelepipedal, etc. The dimensions of the
sides are expressed using @'Data.Ordinal.Ordinal'@ types and the type
constructor @'Data.TypeList.MultiIndex.:|:'@,
e.g. @('Data.Ordinal.Two' 'Data.TypeList.MultiIndex.:|:'
('Data.Ordinal.Three' 'Data.TypeList.MultiIndex.:|:'
'Data.TypeList.MultiIndex.Nil'))@ is a rectangular index set with 2
rows and 3 columns. The index set also contains elements, for example
@('Data.Ordinal.Two' 'Data.TypeList.MultiIndex.:|:'
('Data.Ordinal.Three' 'Data.TypeList.MultiIndex.:|:'
'Data.TypeList.MultiIndex.Nil'))@ contains all the pairs @(i
'Data.TypeList.MultiIndex.:|:' (j 'Data.TypeList.MultiIndex.:|:'
Nil))@ where i is in @'Data.Ordinal.Two'@ and j is in
@'Data.Ordinal.Three'@. See "Data.TypeList.MultiIndex" for more
details.

[@'Data.Tensor.Tensor'@] It is an assignment of elements to each
element of its @'Data.TypeList.MultiIndex.MultiIndex'@.

Objects like vectors and matrices are special cases of tensors.  Most of
the functions to manipulate tensors are grouped into type classes.  This
allow the possibility of having different internal representations
(backends) of a tensor, and act on these with the same functions. At the
moment we provide two backends: one in "Data.Tensor.Pure" (not complete)
that uses a recursive definition, and another in "Data.Tensor.Vector"
that is based on <http://hackage.haskell.org/package/vector> and is
faster. More backends (e.g. one based on
<http://hackage.haskell.org/package/repa>) are planned for future
releases.

Here is a usage example:

>>> :m Data.Tensor.Vector
>>> fromList [2,3,5,1,3,6,0,5,4,2,1,3] :: Tensor (Four :|: Three :|: Nil) Int
[[2,3,5],[1,3,6],[0,5,4],[2,1,3]]

The above defines a tensor with 4 rows and 3 columns (a matrix) and
@'Int'@ coefficients. The entries of this matrix are taken from a list
using @'Data.Tensor.fromList'@ which is a method of the class
@'Data.Tensor.FromList'@. Notice the output: the @'Show'@ instance is
defined in such a way to give a readable representation as list of
lists. The is equivalent but slightly more readable code:

>>> fromList [2,3,5,1,3,6,0,5,4,2,1,3] :: Matrix Four Three Int
[[2,3,5],[1,3,6],[0,5,4],[2,1,3]]

Analogously

>>> fromList [7,3,-6] :: Tensor (Three :|: Nil) Int
[7,3,-6]

and

>>> fromList [7,3,-6] :: Vector Three Int
[7,3,-6]

are the same. In order to access an entry of a @'Data.Tensor.Tensor'@
we use the @'Data.Tensor.!'@ operator, which takes the same
@'Data.TypeList.MultiIndex.MultiIndex'@ of the @'Data.Tensor.Tensor'@
as its second argument:

>>> let a = fromList [2,3,5,1,3,6,0,5,4,2,1,3] :: Matrix Four Three Int
>>> let b = fromList [7,3,-6] :: Vector Three Int
>>> a ! (toMultiIndex [1,3] :: (Four :|: Three :|: Nil))
5
>>> b ! (toMultiIndex [2] :: (Three :|: Nil))
3

it returns the element at the coordinate (1,3) of the matrix @a@, and
the element at the coordinate 2 of the vector b. In fact, thanks to
type inference, we could simply write

>>> a ! toMultiIndex [1,3]
5
>>> b ! toMultiIndex [2]
2

And now a couple of examples of algebraic operations (requires adding
@"Data.Tensor.LinearAlgebra"@ to the import list):

>>> :m Data.Tensor.Vector Data.Tensor.LinearAlgebra
>>> let a = fromList [2,3,5,1,3,6,0,5,4,2,1,3] :: Matrix Four Three Int
>>> let b = fromList [7,3,-6] :: Vector Three Int
>>> a .*. b
[-7,-20,-9,-1]

is the product of matrix @a@ and vector @b@, while

>>> let c = fromList [3,4,0,-1,4,5,6,2,1] :: Matrix Three Three Int
>>> c
[[3,4,0],[-1,4,5],[6,2,1]]
>>> charPoly c
[106,13,8]

gives the coefficients of the characteristic polynomial of the matrix
@c@.

-}

module Data.Tensor.Examples where

