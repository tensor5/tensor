module Data.TypeList.MultiIndex.Internal where

-- | Transform the position indicated by an index array inside an
-- array of given dimensions into the position inside a linear array,
-- starting at 0. For example, in an array of 3 rows and 4 columns,
-- the position [2,2] becomes 5. The indices in the dimension array
-- must be >= 1, the index array must have the same size of the
-- dimension array, and each index in the index array must be >= 1 and
-- <= then the corresponding index in the dimension array.
linearize :: Num n =>
             [n] -- ^ Dimension array
          -> [n] -- ^ Index array
          -> n
linearize ds is = go ds is 0
    where go es js acc = case js of
                           [] -> acc
                           x:xs -> let t = tail es in
                                   go t xs (acc + (x - 1) * product t)

-- | Inverse of @'linearize'@: @('linearize' ds) . ('unlinearize' ds)
-- == 'id'@, @('unlinearize' ds) . ('linearize' ds) == 'id'@.
unlinearize :: Integral n =>
               [n] -- ^ Dimension array
            -> n -- ^ Linearized position (starting from 0)
            -> [n]
unlinearize ds i = go ds i []
    where go es j acc = case es of
                          [] -> acc
                          _:xs -> let (q,r) = quotRem j (product xs) in
                                  go xs r (acc ++ [q + 1])
