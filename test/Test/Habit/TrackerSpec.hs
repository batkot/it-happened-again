{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Habit.TrackerSpec 
    ( test_tracker
    ) where

import qualified Habit.Tracker as HT

import qualified Data.Time as DT
import qualified Data.Time.Calendar as DTC

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary, arbitrary, oneof, listOf)

import Test.DomainDrivenDesign (given, expectFailure, expectSingleEvent)

test_tracker :: TestTree
test_tracker = testGroup "Tracker aggregate tests" 
    [ testGroup "Invariants"
        [ testGroup "Tracker Creation" 
            [ testProperty "Should return Error when created with no starting command" startingWithNonCreateCommandCausesError
            , testProperty "Should raise Created Event when created" canBeStartedWithCreateCommand
            ]
        , testGroup "Running Tracking" 
            [ testProperty "Should return Error when creation command given" createOnStartedTrackingCausesError
            , testProperty "Should raise event when given track" trackEventOnStartedTrackingRaisesEvent
            , testProperty "Should raise finished event on finish" finishOnStartedTrackingRaisesEvent
            ]
        , testGroup "Archived Tracking"
            [ testProperty "Should return error given any command" archivedTrackingRaisesError
            ]
        ]
    ]

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

-- 
newtype EventsForRunningTracking = EventsForRunningTracking { ev :: [HT.Event] }
    deriving Show
instance Arbitrary EventsForRunningTracking where
    arbitrary = do
        created <- HT.Created <$> arbitrary <*> arbitrary
        list <- listOf (HT.Happened <$> arbitrary <*> arbitrary)
        return $ EventsForRunningTracking (list ++ [created])

createOnStartedTrackingCausesError :: HT.TrackingId -> String -> Bool
createOnStartedTrackingCausesError trackingId description = 
    expectFailure st createCmd HT.Error
  where
    st :: HT.Tracking
    st = given [HT.Created trackingId description]
    createCmd = HT.Create trackingId description

trackEventOnStartedTrackingRaisesEvent :: EventsForRunningTracking -> DT.UTCTime -> Maybe HT.GeoCords -> Bool
trackEventOnStartedTrackingRaisesEvent e time place = 
    expectSingleEvent st cmd expectedEvent
  where
    st :: HT.Tracking
    st = given . ev $ e
    cmd = HT.Track time place
    expectedEvent = HT.Happened time place

finishOnStartedTrackingRaisesEvent :: EventsForRunningTracking -> DT.UTCTime -> Bool
finishOnStartedTrackingRaisesEvent e time =
    expectSingleEvent st cmd expectedEvent
  where
    st :: HT.Tracking
    st = given . ev $ e
    cmd = HT.Finish time
    expectedEvent = HT.Finished time

instance Arbitrary HT.Command where
    arbitrary = oneof
        [ HT.Create <$> arbitrary <*> arbitrary
        , HT.Track <$> arbitrary <*> arbitrary
        , HT.Finish <$> arbitrary
        ]

archivedTrackingRaisesError :: EventsForRunningTracking -> DT.UTCTime -> HT.Command -> Bool
archivedTrackingRaisesError e finishedTime cmd =
    expectFailure st cmd HT.TrackingAlreadyClosed
  where
    events = HT.Finished finishedTime : (ev e)
    st :: HT.Tracking
    st = given events
