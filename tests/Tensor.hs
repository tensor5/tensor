{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tensor ( tests ) where

import           Control.Applicative               (liftA2)
import           Distribution.TestSuite.QuickCheck
import           Prelude                           hiding (concat)
import           Prelude.Unicode
import           Test.QuickCheck                   (Arbitrary (arbitrary))

import           Data.Indexable
import           Data.MultiIndex
import           Data.Sliceable
import           Data.Tensor


tests ∷ IO [Test]
tests = return [appendTest, appendTest', sliceTest, reverseTest]

appendTest ∷ Test
appendTest = testGroup "Append and Split"
             [ testProperty "2 3" (\x y → split SOne (append SOne (x ∷ Vector ('S 'One) Int) (y ∷ Vector ('S ('S 'One)) Int)) ≡ (x,y))
             , testProperty "2 3" (\x y → split SOne (append SOne (x ∷ Matrix ('S 'One) ('S ('S 'One)) Int) (y ∷ Matrix ('S ('S 'One)) ('S ('S 'One)) Int)) ≡ (x,y))
             ]

appendTest' ∷ Test
appendTest' = testGroup "Append and |:"
              [ testProperty "[3] [2,3]" (\x y → append SOne (t1 x) y ≡ (x ∷ Vector ('S ('S 'One)) Int) |: (y ∷ Matrix ('S 'One) ('S ('S 'One)) Int))
              ]

sliceTest ∷ Test
sliceTest = testGroup "Slice"
            [ testProperty "[]" (\x → slice (x ∷ Tensor '[] Int) nilS ≡ x)
            , testProperty "[2,3] [Nothing, Nothing]" (\x → slice (x ∷ Matrix ('S 'One) ('S ('S 'One)) Int) (allCons $ allCons nilS) ≡ x)
            ]

reverseTest ∷ Test
reverseTest = testGroup "Reverse"
              [ testProperty "rev Tensor [] (Tensor [] Int)"
                (\x → rev (x ∷ Tensor '[] (Tensor '[] Int)) ≡ unT0 x)
              , testProperty "rev Tensor [] (Tensor [2] Int)"
                (\x → rev (x ∷ Tensor '[] (Vector ('S 'One) Int)) ≡ unT0 x)
              , testProperty "rev Tensor [] (Tensor [2,3] Int)"
                (\x → rev (x ∷ Tensor '[] (Matrix ('S 'One) ('S ('S 'One)) Int)) ≡ unT0 x)
              , testProperty "rev Tensor [2] (Tensor [] Int)"
                (\x → rev (x ∷ Vector ('S 'One) (Tensor '[] Int)) ≡ unT0 (concat x))
              , testProperty "rev Tensor [2] (Tensor [3] Int)"
                (\x → rev (x ∷ Vector ('S 'One) (Vector ('S ('S 'One)) Int)) ≡ unT0 (concat x))
              , testProperty "rev Tensor [2] (Tensor [3,4] Int)"
                (\x → rev (x ∷ Vector ('S 'One) (Matrix ('S ('S 'One)) ('S ('S ('S 'One))) Int)) ≡ unT0 (concat x))
              , testProperty "rev Tensor [2,3] (Tensor [] Int)"
                (\x → rev (x ∷ Matrix ('S 'One) ('S ('S 'One)) (Tensor '[] Int)) ≡ unT0 (concat $ concat x))
              , testProperty "rev Tensor [2,3] (Tensor [4] Int)"
                (\x → rev (x ∷ Matrix ('S 'One) ('S ('S 'One)) (Vector ('S ('S ('S 'One))) Int)) ≡ unT0 (concat $ concat x))
              , testProperty "rev Tensor [2,3] (Tensor [4,5] Int)"
                (\x → rev (x ∷ Matrix ('S 'One) ('S ('S 'One)) (Matrix ('S ('S ('S 'One))) ('S ('S ('S ('S 'One)))) Int)) ≡ unT0 (concat $ concat x))
              , testProperty "unRev Tensor [] (Tensor [] Int)"
                (\x → unRev (x ∷ Tensor '[] (Tensor '[] Int)) ≡ fmap unT0 x)
              , testProperty "unRev Tensor [] (Tensor [2] Int)"
                (\x → unRev (x ∷ Tensor '[] (Vector ('S 'One) Int)) ≡ fmap unT0 (unConcat x))
              , testProperty "unRev Tensor [] (Tensor [2,3] Int)"
                (\x → unRev (x ∷ Tensor '[] (Matrix ('S 'One) ('S ('S 'One)) Int)) ≡ fmap unT0 (unConcat $ unConcat x))
              , testProperty "unRev Tensor [2] (Tensor [] Int)"
                (\x → unRev (x ∷ Vector ('S 'One) (Tensor '[] Int)) ≡ fmap unT0 x)
              , testProperty "unRev Tensor [2] (Tensor [3] Int)"
                (\x → unRev (x ∷ Vector ('S 'One) (Vector ('S ('S 'One)) Int)) ≡ fmap unT0 (unConcat x))
              , testProperty "unRev Tensor [2] (Tensor [3,4] Int)"
                (\x → unRev (x ∷ Vector ('S 'One) (Matrix ('S ('S 'One)) ('S ('S ('S 'One))) Int)) ≡ fmap unT0 (unConcat $ unConcat x))
              , testProperty "unRev Tensor [2,3] (Tensor [] Int)"
                (\x → unRev (x ∷ Matrix ('S 'One) ('S ('S 'One)) (Tensor '[] Int)) ≡ fmap unT0 x)
              , testProperty "unRev Tensor [2,3] (Tensor [4] Int)"
                (\x → unRev (x ∷ Matrix ('S 'One) ('S ('S 'One)) (Vector ('S ('S ('S 'One))) Int)) ≡ fmap unT0 (unConcat x))
              , testProperty "unRev Tensor [2,3] (Tensor [4,5] Int)"
                (\x → unRev (x ∷ Matrix ('S 'One) ('S ('S 'One)) (Matrix ('S ('S ('S 'One))) ('S ('S ('S ('S 'One)))) Int)) ≡ fmap unT0 (unConcat $ unConcat x))
              ]


instance Arbitrary e ⇒ Arbitrary (Tensor '[] e) where
    arbitrary = t0 <$> arbitrary

instance Arbitrary (Tensor is e) ⇒ Arbitrary (Tensor ('One ': is) e) where
    arbitrary = t1 <$> arbitrary

instance ( Arbitrary (Tensor is e)
         , Arbitrary (Tensor (i ': is) e)
         ) ⇒ Arbitrary (Tensor ('S i ': is) e) where
    arbitrary = liftA2 (:|) arbitrary arbitrary
