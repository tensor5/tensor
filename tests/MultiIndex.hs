module MultiIndex ( tests ) where

import           Distribution.TestSuite.QuickCheck
import           Prelude.Unicode

import           Data.MultiIndex


tests ∷ IO [Test]
tests = return [toListTest, fromListTest]

toListTest ∷ Test
toListTest =
    testGroup "toList"
              [ testProperty "" (toList Nil ≡ ([] ∷ [Int]))
              , testProperty "" (toList (OneCons Nil) ≡ ([1] ∷ [Int]))
              , testProperty "" (toList (HeadSucc $ OneCons Nil) ≡ ([2] ∷ [Int]))
              , testProperty "" (toList (OneCons $ OneCons Nil) ≡ ([1,1] ∷ [Int]))
              , testProperty "" (toList (OneCons $ HeadSucc $ OneCons Nil) ≡ ([1,2] ∷ [Int]))
              , testProperty "" (toList (HeadSucc $ OneCons $ HeadSucc $ OneCons Nil) ≡ ([2,2] ∷ [Int]))
              ]


fromListTest ∷ Test
fromListTest =
    testGroup "fromList"
              [ testProperty "" (fromList ([] ∷ [Int]) ≡ Just Nil) ]
