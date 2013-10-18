--------------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  © 2012-2013 Nicola Squartini
-- License     :  GPL-3
--
-- Maintainer  :  Nicola Squartini <tensor5@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module define a datatype @'Tensor'@ which implements the classes and
-- methods defined in "Data.Tensor" and "Data.Tensor.LinearAlgebra". It is
-- represented internally as a @'Data.Vector.Vector'@.
--
--------------------------------------------------------------------------------

module Data.Tensor.Vector
    ( Tensor
    , Vector
    , Matrix
    , ColumnVector
    , vector2ColumnVector
    , columnVector2Vector
    , RowVector
    , vector2RowVector
    , rowVector2Vector

    , fromVector

    , module Data.Tensor
    , module Data.TypeList.MultiIndex

    ) where


import Data.Tensor hiding (Tensor)
import Data.Tensor.Vector.Internal
import Data.TypeList.MultiIndex
