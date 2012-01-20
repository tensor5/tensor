module Data.Tensor.Vector
    (Tensor,
     ColumnVector,
     Vector,
     RowVector,
     Matrix,

     FromVector(..),

     module Data.Tensor

    ) where


import Data.Tensor hiding (Tensor)
import Data.Tensor.Vector.Internal
