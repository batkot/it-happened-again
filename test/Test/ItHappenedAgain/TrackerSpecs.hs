module Test.ItHappenedAgain.TrackerSpecs
    ( test_tracker_mtl
    ) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty)

import Test.DomainDrivenDesign.MTL
import Test.ItHappenedAgain.Arbitrary

import Data.Time

import ItHappenedAgain.Tracker

test_tracker_mtl :: TestTree
test_tracker_mtl = testGroup "MTL Tracker aggregate tests"
    [ testGroup "Tracker creation"
        [ testProperty "Given existing aggregate should raise error" createForExistingAggregateRaisesError
        , testProperty "Given tracking doesnt exist should raise event" createWhenAggregateDoesntExistsRaisesEvent
        ]
    , testGroup "Track new event"
        [ testProperty "Given not existing tracking should raise error" trackWhenAggregateDoesntExistsRaisesError
        , testProperty "Given archived tracking should raise error" trackWhenTrackArchivedRaisesError
        , testProperty "Given running tracking should raise event" trackOnRunningTrackRaisesEvent
        ]
    , testGroup "Finish tracking"
        [ testProperty "Given not existing tracking should raise error" finishWhenAggregateDoesntExistsRaisesError
        , testProperty "Given archived tracking should raise error" finishWhenTrackArchivedRaisesError
        , testProperty "Given running tracking should raise event" finishOnRunningTrackRaisesEvent
        ]
    ]

createForExistingAggregateRaisesError :: EventsForRunningTracking -> TrackingId -> RandomText -> Bool
createForExistingAggregateRaisesError events trackId (RandomText trackName) =
    given (runningEvents events) `when` create trackId trackName `expect` Left TrackingAlreadyExists

createWhenAggregateDoesntExistsRaisesEvent :: TrackingId -> RandomText -> Bool
createWhenAggregateDoesntExistsRaisesEvent trackId (RandomText trackName) =
    given [] `when` create trackId trackName `expect` (Right [Created trackId trackName] :: Either Error [Event])

trackWhenAggregateDoesntExistsRaisesError :: UTCTime -> Maybe GeoCords -> Bool
trackWhenAggregateDoesntExistsRaisesError eventTime eventPlace =
    given [] `when` track eventTime eventPlace `expect` Left TrackingNotFound

trackWhenTrackArchivedRaisesError :: ClosedTrackingEvents -> UTCTime -> Maybe GeoCords -> Bool
trackWhenTrackArchivedRaisesError events eventTime eventPlace =
    given (closedEvents events) `when` track eventTime eventPlace `expect` Left TrackingAlreadyClosed

trackOnRunningTrackRaisesEvent :: EventsForRunningTracking -> UTCTime -> Maybe GeoCords -> Bool
trackOnRunningTrackRaisesEvent events eventTime eventPlace =
    given (runningEvents events) `when` track eventTime eventPlace `expect` (Right [Happened eventTime eventPlace] :: Either Error [Event])

finishWhenAggregateDoesntExistsRaisesError :: UTCTime -> Bool
finishWhenAggregateDoesntExistsRaisesError finishTime =
    given [] `when` finish finishTime `expect` Left TrackingNotFound

finishWhenTrackArchivedRaisesError :: ClosedTrackingEvents -> UTCTime -> Bool
finishWhenTrackArchivedRaisesError events finishTime =
    given (closedEvents events) `when` finish finishTime `expect` Left TrackingAlreadyClosed

finishOnRunningTrackRaisesEvent :: EventsForRunningTracking -> UTCTime -> Bool
finishOnRunningTrackRaisesEvent events finishTime =
    given (runningEvents events) `when` finish finishTime `expect` (Right [Finished finishTime] :: Either Error [Event])
