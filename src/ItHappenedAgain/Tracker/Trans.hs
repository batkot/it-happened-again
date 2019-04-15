{-# LANGUAGE LambdaCase #-}

module ItHappenedAgain.Tracker.Trans 
    ( create
    , track
    , finish
    ) where

import Data.Time (UTCTime)
import Data.Text (Text)

import DomainDrivenDesign.Trans

import ItHappenedAgain.Tracker.Data

type TrackerAggregate = AggregateActionMonad Tracking Event Error
    
create :: TrackingId -> Text -> TrackerAggregate ()
create trackerId trackerName = 
    ensureNew >> raiseEvent (Created trackerId trackerName)

track :: UTCTime -> Maybe GeoCords -> TrackerAggregate ()
track eventTime eventPlace =
    ensureRunning >> raiseEvent (Happened eventTime eventPlace)

finish :: UTCTime -> TrackerAggregate ()
finish finishTime =
    ensureRunning >> raiseEvent (Finished finishTime)

ensureNew :: TrackerAggregate ()
ensureNew =
    getAggregate >>= \case
        Empty -> return ()
        _ -> raiseError TrackingAlreadyExists

ensureRunning :: TrackerAggregate ()
ensureRunning =
    getAggregate >>= \case
        Running _ -> return ()
        Archived _ -> raiseError TrackingAlreadyClosed
        Empty -> raiseError TrackingNotFound
