module Test.Spec where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty)


test_a :: TestTree
test_a = testGroup "My Tests" 
    [ testGroup "Quick Check" 
        [ testProperty "Will fail" prop_fooBar
        , testProperty "This will pass" prop_pass 
        ]
    , testGroup "Other things" [ testProperty "Some another property"  prop_pass]
    ]

prop_fooBar :: Int -> Bool
prop_fooBar = const True

prop_pass :: Int -> Bool
prop_pass x = x == x
