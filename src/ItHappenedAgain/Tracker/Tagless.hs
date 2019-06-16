{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module ItHappenedAgain.Tracker.Tagless
    ( create
    , track
    , finish
    ) where

import Data.Time (UTCTime)
import Data.Text (Text)

import DomainDrivenDesign.Tagless

import ItHappenedAgain.Tracker.Data 

instance Aggregate Tracking Event Error 

create
    :: MonadAggregateAction Tracking Event Error m
    => TrackingId
    -> Text
    -> m ()
create trackerId trackerName = 
    ensureNew >> raiseEvent (Created trackerId trackerName)

track
    :: MonadAggregateAction Tracking Event Error m
    => UTCTime 
    -> Maybe GeoCords
    -> m ()
track eventTime eventPlace =
    ensureRunning >> raiseEvent (Happened eventTime eventPlace)

finish 
    :: MonadAggregateAction Tracking Event Error m
    => UTCTime 
    -> m ()
finish finishTime =
    ensureRunning >> raiseEvent (Finished finishTime)

ensureNew 
    :: MonadAggregateAction Tracking Event Error m
    => m ()
ensureNew =
    getAggregate >>= \case
        Empty -> return ()
        _ -> raiseError TrackingAlreadyExists

ensureRunning
    :: MonadAggregateAction Tracking Event Error m
    => m ()
ensureRunning =
    getAggregate >>= \case
        Running _ -> return ()
        Archived _ -> raiseError TrackingAlreadyClosed
        Empty -> raiseError TrackingNotFound
