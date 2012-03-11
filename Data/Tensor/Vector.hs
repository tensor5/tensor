module Data.Tensor.Vector
    (Tensor,
     Vector,
     Matrix,
     ColumnVector,
     vector2ColumnVector,
     columnVector2Vector,
     RowVector,
     vector2RowVector,
     rowVector2Vector,

     FromVector(..),

     module Data.Tensor

    ) where


import Data.Tensor hiding (Tensor)
import Data.Tensor.Vector.Internal
