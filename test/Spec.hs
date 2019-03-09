import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "My Tests" 
    [ testGroup "Quick Check" 
        [ testProperty "Will fail" prop_foo
        , testProperty "This will pass" pass 
        ]
    , testGroup "Other things" [ testProperty "Some another property"  pass]
    ]

prop_foo :: Int -> Bool
prop_foo = (==) 2

pass :: Int -> Bool
pass x = x == x
