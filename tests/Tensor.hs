{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tensor ( tests ) where

import           Control.Applicative               (liftA2, (<$>))
import           Distribution.TestSuite.QuickCheck
import           Prelude.Unicode
import           Test.QuickCheck                   (Arbitrary (arbitrary))

import           Data.MultiIndex
import           Data.Sliceable
import           Data.Tensor


tests ∷ IO [Test]
tests = return [appendTest, appendTest', sliceTest]

appendTest ∷ Test
appendTest = testGroup "Append and Split"
             [ testProperty "2 3" (\x y → split SOne (append SOne (x ∷ Vector (S One) Int) (y ∷ Vector (S (S One)) Int)) ≡ (x,y))
             , testProperty "2 3" (\x y → split SOne (append SOne (x ∷ Matrix (S One) (S (S One)) Int) (y ∷ Matrix (S (S One)) (S (S One)) Int)) ≡ (x,y))
             ]

appendTest' ∷ Test
appendTest' = testGroup "Append and |:"
              [ testProperty "[3] [2,3]" (\x y → append SOne (t1 x) y ≡ (x ∷ Vector (S (S One)) Int) |: (y ∷ Matrix (S One) (S (S One)) Int))
              ]

sliceTest ∷ Test
sliceTest = testGroup "Slice"
            [ testProperty "[]" (\x → slice (x ∷ Tensor '[] Int) nilS ≡ x)
            , testProperty "[2,3] [Nothing, Nothing]" (\x → slice (x ∷ Matrix (S One) (S (S One)) Int) (allCons $ allCons nilS) ≡ x)
            ]


instance Arbitrary e ⇒ Arbitrary (Tensor '[] e) where
    arbitrary = t0 <$> arbitrary

instance Arbitrary (Tensor is e) ⇒ Arbitrary (Tensor (One ': is) e) where
    arbitrary = t1 <$> arbitrary

instance ( Arbitrary (Tensor is e)
         , Arbitrary (Tensor (i ': is) e)
         ) ⇒ Arbitrary (Tensor (S i ': is) e) where
    arbitrary = liftA2 (:|) arbitrary arbitrary
