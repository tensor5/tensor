{-# LANGUAGE TypeOperators #-}

module Main where

import           Data.Ordinal
import           Data.TypeList.MultiIndex
import           Prelude                  hiding (reverse)
import           System.Exit
import           System.Random
import           Test.QuickCheck

instance Arbitrary Nil where
    arbitrary = elements [Nil]

instance (Bounded e, Random e, Bounded l, Random l) => Arbitrary (e :|: l) where
    arbitrary = arbitraryBoundedRandom

main :: IO ()
main = do
  cs <- sequence
       [ quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: Nil -> Bool)
       , quickCheckResult ((maxBound :: Nil) == toMultiIndex ([] :: [Int]))
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (One :|: Nil) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Two :|: Nil) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Three :|: Nil) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Four :|: Nil) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Five :|: Nil) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Six :|: Nil) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Seven :|: Nil) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Eight :|: Nil) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Nine :|: Nil) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Ten :|: Nil) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (One :|: (One :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (One :|: (Two :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (One :|: (Three :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (One :|: (Four :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (One :|: (Five :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Two :|: (One :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Two :|: (Two :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Two :|: (Three :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Two :|: (Four :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Two :|: (Five :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Three :|: (One :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Three :|: (Two :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Three :|: (Three :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Three :|: (Four :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Three :|: (Five :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Four :|: (One :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Four :|: (Two :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Four :|: (Three :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Four :|: (Four :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Four :|: (Five :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Five :|: (One :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Five :|: (Two :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Five :|: (Three :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Five :|: (Four :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Five :|: (Five :|: Nil)) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (One :|: (One :|: (One :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (One :|: (One :|: (Two :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (One :|: (One :|: (Three :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (One :|: (Two :|: (One :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (One :|: (Two :|: (Two :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (One :|: (Two :|: (Three :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (One :|: (Three :|: (One :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (One :|: (Three :|: (Two :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (One :|: (Three :|: (Three :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Two :|: (One :|: (One :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Two :|: (One :|: (Two :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Two :|: (One :|: (Three :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Two :|: (Two :|: (One :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Two :|: (Two :|: (Two :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Two :|: (Two :|: (Three :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Two :|: (Three :|: (One :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Two :|: (Three :|: (Two :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Two :|: (Three :|: (Three :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Three :|: (One :|: (One :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Three :|: (One :|: (Two :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Three :|: (One :|: (Three :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Three :|: (Two :|: (One :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Three :|: (Two :|: (Two :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Three :|: (Two :|: (Three :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Three :|: (Three :|: (One :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Three :|: (Three :|: (Two :|: Nil))) -> Bool)
       , quickCheckResult ((\x ->
                                toMultiIndex (fromMultiIndex x :: [Int]) == x)
                           :: (Three :|: (Three :|: (Three :|: Nil))) -> Bool)
       , quickCheckResult ((maxBound :: One :|: Nil) ==
                           toMultiIndex ([1] :: [Int]))
       , quickCheckResult ((maxBound :: Two :|: Nil) ==
                           toMultiIndex ([2] :: [Int]))
       , quickCheckResult ((maxBound :: Three :|: Nil) ==
                           toMultiIndex ([3] :: [Int]))
       , quickCheckResult ((maxBound :: Four :|: Nil) ==
                           toMultiIndex ([4] :: [Int]))
       , quickCheckResult ((maxBound :: Five :|: Nil) ==
                           toMultiIndex ([5] :: [Int]))
       , quickCheckResult ((maxBound :: Six :|: Nil) ==
                           toMultiIndex ([6] :: [Int]))
       , quickCheckResult ((maxBound :: Seven :|: Nil) ==
                           toMultiIndex ([7] :: [Int]))
       , quickCheckResult ((maxBound :: Eight :|: Nil) ==
                           toMultiIndex ([8] :: [Int]))
       , quickCheckResult ((maxBound :: Nine :|: Nil) ==
                           toMultiIndex ([9] :: [Int]))
       , quickCheckResult ((maxBound :: Ten :|: Nil) ==
                           toMultiIndex ([10] :: [Int]))
       , quickCheckResult ((maxBound :: One :|: (One :|: Nil))
                           == toMultiIndex ([1,1] :: [Int]))
       , quickCheckResult ((maxBound :: One :|: (Two :|: Nil))
                           == toMultiIndex ([1,2] :: [Int]))
       , quickCheckResult ((maxBound :: One :|: (Three :|: Nil))
                           == toMultiIndex ([1,3] :: [Int]))
       , quickCheckResult ((maxBound :: One :|: (Four :|: Nil))
                           == toMultiIndex ([1,4] :: [Int]))
       , quickCheckResult ((maxBound :: One :|: (Five :|: Nil))
                           == toMultiIndex ([1,5] :: [Int]))
       , quickCheckResult ((maxBound :: Two :|: (One :|: Nil))
                           == toMultiIndex ([2,1] :: [Int]))
       , quickCheckResult ((maxBound :: Two :|: (Two :|: Nil))
                           == toMultiIndex ([2,2] :: [Int]))
       , quickCheckResult ((maxBound :: Two :|: (Three :|: Nil))
                           == toMultiIndex ([2,3] :: [Int]))
       , quickCheckResult ((maxBound :: Two :|: (Four :|: Nil))
                           == toMultiIndex ([2,4] :: [Int]))
       , quickCheckResult ((maxBound :: Two :|: (Five :|: Nil))
                           == toMultiIndex ([2,5] :: [Int]))
       , quickCheckResult ((maxBound :: Three :|: (One :|: Nil))
                           == toMultiIndex ([3,1] :: [Int]))
       , quickCheckResult ((maxBound :: Three :|: (Two :|: Nil))
                           == toMultiIndex ([3,2] :: [Int]))
       , quickCheckResult ((maxBound :: Three :|: (Three :|: Nil))
                           == toMultiIndex ([3,3] :: [Int]))
       , quickCheckResult ((maxBound :: Three :|: (Four :|: Nil))
                           == toMultiIndex ([3,4] :: [Int]))
       , quickCheckResult ((maxBound :: Three :|: (Five :|: Nil))
                           == toMultiIndex ([3,5] :: [Int]))
       , quickCheckResult ((maxBound :: Four :|: (One :|: Nil))
                           == toMultiIndex ([4,1] :: [Int]))
       , quickCheckResult ((maxBound :: Four :|: (Two :|: Nil))
                           == toMultiIndex ([4,2] :: [Int]))
       , quickCheckResult ((maxBound :: Four :|: (Three :|: Nil))
                           == toMultiIndex ([4,3] :: [Int]))
       , quickCheckResult ((maxBound :: Four :|: (Four :|: Nil))
                           == toMultiIndex ([4,4] :: [Int]))
       , quickCheckResult ((maxBound :: Four :|: (Five :|: Nil))
                           == toMultiIndex ([4,5] :: [Int]))
       , quickCheckResult ((maxBound :: Five :|: (One :|: Nil))
                           == toMultiIndex ([5,1] :: [Int]))
       , quickCheckResult ((maxBound :: Five :|: (Two :|: Nil))
                           == toMultiIndex ([5,2] :: [Int]))
       , quickCheckResult ((maxBound :: Five :|: (Three :|: Nil))
                           == toMultiIndex ([5,3] :: [Int]))
       , quickCheckResult ((maxBound :: Five :|: (Four :|: Nil))
                           == toMultiIndex ([5,4] :: [Int]))
       , quickCheckResult ((maxBound :: Five :|: (Five :|: Nil))
                           == toMultiIndex ([5,5] :: [Int]))
       , quickCheckResult ((\x -> x == reverse (reverse x)) :: Nil -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: One :|: Nil -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Two :|: Nil -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Three :|: Nil -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Four :|: Nil -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Five :|: Nil -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: One :|: (One :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: One :|: (Two :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: One :|: (Three :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: One :|: (Four :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: One :|: (Five :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Two :|: (One :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Two :|: (Two :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Two :|: (Three :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Two :|: (Four :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Two :|: (Five :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Three :|: (One :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Three :|: (Two :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Three :|: (Three :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Three :|: (Four :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Three :|: (Five :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Four :|: (One :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Four :|: (Two :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Four :|: (Three :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Four :|: (Four :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Four :|: (Five :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Five :|: (One :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Five :|: (Two :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Five :|: (Three :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Five :|: (Four :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Five :|: (Five :|: Nil) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: One :|: (One :|: (One :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: One :|: (One :|: (Two :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: One :|: (One :|: (Three :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: One :|: (Two :|: (One :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: One :|: (Two :|: (Two :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: One :|: (Two :|: (Three :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: One :|: (Three :|: (One :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: One :|: (Three :|: (Two :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: One :|: (Three :|: (Three :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Two :|: (One :|: (One :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Two :|: (One :|: (Two :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Two :|: (One :|: (Three :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Two :|: (Two :|: (One :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Two :|: (Two :|: (Two :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Two :|: (Two :|: (Three :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Two :|: (Three :|: (One :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Two :|: (Three :|: (Two :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Two :|: (Three :|: (Three :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Three :|: (One :|: (One :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Three :|: (One :|: (Two :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Three :|: (One :|: (Three :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Three :|: (Two :|: (One :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Three :|: (Two :|: (Two :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Three :|: (Two :|: (Three :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Three :|: (Three :|: (One :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Three :|: (Three :|: (Two :|: Nil)) -> Bool)
       , quickCheckResult ((\x -> x == reverse (reverse x))
                               :: Three :|: (Three :|: (Three :|: Nil)) -> Bool)
       ]
  if checkRes cs
    then exitSuccess
    else exitFailure


checkRes :: [Result] -> Bool
checkRes = all $ \x -> case x of
                          Success{} -> True
                          _ -> False

