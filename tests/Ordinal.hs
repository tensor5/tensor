module Main where

import           Data.Ordinal
import           System.Exit
import           Test.QuickCheck

instance Arbitrary One where
    arbitrary = elements [One]
    shrink _ = []

instance (Bounded n, Ordinal n) => Arbitrary (Succ n) where
    arbitrary = arbitraryBoundedRandom
    shrink n = map toOrdinal (shrink (fromOrdinal n :: Integer))

main :: IO ()
main = do
  cs <- sequence
        [ quickCheckResult ((\n -> toOrdinal (fromOrdinal n :: Int) == n)
                                :: One -> Bool)
        , quickCheckResult ((maxBound :: One) == toOrdinal (1 :: Int))
        , quickCheckResult ((\n -> toOrdinal (fromOrdinal n :: Int) == n)
                                :: Two -> Bool)
        , quickCheckResult ((maxBound :: Two) == toOrdinal (2 :: Int))
        , quickCheckResult ((\n -> toOrdinal (fromOrdinal n :: Int) == n)
                                :: Three -> Bool)
        , quickCheckResult ((maxBound :: Three) == toOrdinal (3 :: Int))
        , quickCheckResult ((\n -> toOrdinal (fromOrdinal n :: Int) == n)
                                :: Four -> Bool)
        , quickCheckResult ((maxBound :: Four) == toOrdinal (4 :: Int))
        , quickCheckResult ((\n -> toOrdinal (fromOrdinal n :: Int) == n)
                                :: Five -> Bool)
        , quickCheckResult ((maxBound :: Five) == toOrdinal (5 :: Int))
        , quickCheckResult ((\n -> toOrdinal (fromOrdinal n :: Int) == n)
                                :: Six -> Bool)
        , quickCheckResult ((maxBound :: Six) == toOrdinal (6 :: Int))
        , quickCheckResult ((\n -> toOrdinal (fromOrdinal n :: Int) == n)
                                :: Seven -> Bool)
        , quickCheckResult ((maxBound :: Seven) == toOrdinal (7 :: Int))
        , quickCheckResult ((\n -> toOrdinal (fromOrdinal n :: Int) == n)
                                :: Eight -> Bool)
        , quickCheckResult ((maxBound :: Eight) == toOrdinal (8 :: Int))
        , quickCheckResult ((\n -> toOrdinal (fromOrdinal n :: Int) == n)
                                :: Nine -> Bool)
        , quickCheckResult ((maxBound :: Nine) == toOrdinal (9 :: Int))
        , quickCheckResult ((\n -> toOrdinal (fromOrdinal n :: Int) == n)
                                :: Ten -> Bool)
        , quickCheckResult ((maxBound :: Ten) == toOrdinal (10 :: Int))
        ]
  if checkRes cs
    then exitSuccess
    else exitFailure


checkRes :: [Result] -> Bool
checkRes = all $ \x -> case x of
                          Success{} -> True
                          _ -> False

