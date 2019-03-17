{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Habit.TrackerSpec 
    ( test_tracker
    ) where

import qualified Habit.Tracker as HT

import qualified Data.Time as DT
import qualified Data.Time.Calendar as DTC

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary, arbitrary, oneof)

import Test.DomainDrivenDesign (given, expectFailure, expectSingleEvent)

test_tracker :: TestTree
test_tracker = testGroup "Tracker aggregate tests" 
    [ testGroup "Invariants"
        [ testGroup "Tracker Creation" 
            [ testProperty "Should return Error when created with no starting command" startingWithNonCreateCommandCausesError
            , testProperty "Should raise Created Event when created" canBeStartedWithCreateCommand
            ]
        , testGroup "Tracking event" 
            [ 
            ]
        ]
    , testGroup "State apply"
        [
        ]
    ]

-- Given init State can only be created
newtype NonStartingCommand = NonStartingCommand { getCommand :: HT.Command } 
    deriving Show

instance Arbitrary DT.UTCTime where
    arbitrary = DT.UTCTime <$> dayGen <*> diffTimeGen
      where
        dayGen = DTC.fromGregorian <$> arbitrary <*> arbitrary <*> arbitrary
        diffTimeGen = DT.secondsToDiffTime <$> arbitrary

instance Arbitrary HT.TrackingId where
    arbitrary = HT.TrackingId <$> arbitrary

instance Arbitrary HT.GeoCords where
    arbitrary = HT.GeoCords <$> arbitrary

instance Arbitrary NonStartingCommand where
    arbitrary = fmap NonStartingCommand $ oneof 
        [ HT.Track <$> arbitrary <*> arbitrary 
        , HT.Finish <$> arbitrary ]

startingWithNonCreateCommandCausesError :: NonStartingCommand -> Bool
startingWithNonCreateCommandCausesError (NonStartingCommand cmd) = 
    expectFailure st cmd HT.Error
  where
    st :: HT.Tracking
    st = given []

canBeStartedWithCreateCommand :: HT.TrackingId -> String -> Bool
canBeStartedWithCreateCommand trackingId description =
    expectSingleEvent st createCmd expectedEvent
  where
    st :: HT.Tracking
    st = given []
    createCmd = HT.Create trackingId description
    expectedEvent = HT.Created trackingId description
