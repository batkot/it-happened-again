{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.ItHappenedAgain.Tracker.SillySpecs
    ( test_tracker
    ) where

import ItHappenedAgain.Tracker.Silly
import qualified ItHappenedAgain.Tracker.Data as HT

import qualified Data.Time as DT
import Data.Text (pack)

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary, arbitrary, oneof)

import Test.DomainDrivenDesign.Silly (given, expectFailure, expectSingleEvent)
import Test.ItHappenedAgain.Tracker.Arbitrary

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

newtype NonStartingCommand = NonStartingCommand { getCommand :: Command } 
    deriving Show

instance Arbitrary NonStartingCommand where
    arbitrary = fmap NonStartingCommand $ oneof 
        [ Track <$> arbitrary <*> arbitrary 
        , Finish <$> arbitrary ]

startingWithNonCreateCommandCausesError :: NonStartingCommand -> Bool
startingWithNonCreateCommandCausesError (NonStartingCommand cmd) = 
    expectFailure st cmd HT.Error
  where
    st :: HT.Tracking
    st = given []

canBeStartedWithCreateCommand :: HT.TrackingId -> RandomText -> Bool
canBeStartedWithCreateCommand trackingId (RandomText trackName) =
    expectSingleEvent st createCmd expectedEvent
  where
    st :: HT.Tracking
    st = given []
    createCmd = Create trackingId trackName
    expectedEvent = HT.Created trackingId trackName

-- 

createOnStartedTrackingCausesError :: HT.TrackingId -> RandomText -> Bool
createOnStartedTrackingCausesError trackingId (RandomText trackName) = 
    expectFailure st createCmd HT.Error
  where
    st :: HT.Tracking
    st = given [HT.Created trackingId trackName]
    createCmd = Create trackingId trackName

trackEventOnStartedTrackingRaisesEvent :: EventsForRunningTracking -> DT.UTCTime -> Maybe HT.GeoCords -> Bool
trackEventOnStartedTrackingRaisesEvent e time place = 
    expectSingleEvent st cmd expectedEvent
  where
    st :: HT.Tracking
    st = given . runningEvents $ e
    cmd = Track time place
    expectedEvent = HT.Happened time place

finishOnStartedTrackingRaisesEvent :: EventsForRunningTracking -> DT.UTCTime -> Bool
finishOnStartedTrackingRaisesEvent e time =
    expectSingleEvent st cmd expectedEvent
  where
    st :: HT.Tracking
    st = given . runningEvents $ e
    cmd = Finish time
    expectedEvent = HT.Finished time

instance Arbitrary Command where
    arbitrary = oneof
        [ Create <$> arbitrary <*> (fmap pack arbitrary)
        , Track <$> arbitrary <*> arbitrary
        , Finish <$> arbitrary
        ]

archivedTrackingRaisesError :: EventsForRunningTracking -> DT.UTCTime -> Command -> Bool
archivedTrackingRaisesError e finishedTime cmd =
    expectFailure st cmd HT.TrackingAlreadyClosed
  where
    events = HT.Finished finishedTime : (runningEvents e)
    st :: HT.Tracking
    st = given events
