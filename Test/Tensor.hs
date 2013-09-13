module Main where

import Data.Ordinal
import Data.Tensor.LinearAlgebra hiding (Matrix)
import Data.Tensor.Vector
import Prelude hiding (replicate)
import System.Exit
import System.Random
import Test.QuickCheck

--instance (Bounded e, MultiIndex i, Random e) => Arbitrary (Tensor i e) where
--    arbitrary = arbitraryBoundedRandom

instance (MultiIndex i, Num e, Random e) => Arbitrary (Tensor i e) where
    arbitrary = choose (replicate (negate 100), replicate 100)

main :: IO ()
main = do
  cs <- sequence
       [ quickCheckResult (\x -> (x :: Matrix Two Three Int) ==
                                 transpose (transpose x))
       , quickCheckResult (\x -> (zero :: Matrix One One Int) ==
                                 polyEval x (altSign (charPoly x ++ [1])))
       , quickCheckResult (\x -> (zero :: Matrix Two Two Int) ==
                                 polyEval x (altSign (charPoly x ++ [1])))
       , quickCheckResult ((\x -> zero == polyEval
                            (elemMap toRational x :: Matrix Two Two Rational)
                            (minPoly
                             (elemMap toRational x :: Matrix Two Two Rational)
                             ++ [1])
                           ) :: (Matrix Two Two Float -> Bool))
       , quickCheckResult (\x -> (zero :: Matrix Three Three Int) ==
                                 polyEval x (altSign (charPoly x ++ [1])))
       , quickCheckResult ((\x -> zero == polyEval
                            (elemMap toRational x :: Matrix Three Three Rational)
                            (minPoly
                             (elemMap toRational x :: Matrix Three Three Rational)
                             ++ [1])
                           ) :: (Matrix Three Three Float -> Bool))
       , quickCheckResult (\x -> (zero :: Matrix Four Four Int) ==
                                 polyEval x (altSign (charPoly x ++ [1])))
       , quickCheckResult ((\x -> zero == polyEval
                            (elemMap toRational x :: Matrix Four Four Rational)
                            (minPoly
                             (elemMap toRational x :: Matrix Four Four Rational)
                             ++ [1])
                           ) :: (Matrix Four Four Float -> Bool))
       , quickCheckResult (\x -> (zero :: Matrix Five Five Int) ==
                                 polyEval x (altSign (charPoly x ++ [1])))
       , quickCheckResult ((\x -> zero == polyEval
                            (elemMap toRational x :: Matrix Five Five Rational)
                            (minPoly
                             (elemMap toRational x :: Matrix Five Five Rational)
                             ++ [1])
                           ) :: (Matrix Five Five Float -> Bool))
       ]
  if checkRes cs
    then exitSuccess
    else exitFailure


altSign :: Num a => [a] -> [a]
altSign = leave
    where leave [] = []
          leave (x:xs) = x : change xs
          change [] = []
          change (x:xs) = negate x : leave xs


checkRes :: [Result] -> Bool
checkRes = all $ \x -> case x of
                          Success{} -> True
                          _ -> False

