{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module ItHappenedAgain.Tracker.MTL 
    ( create
    , track
    , finish
    ) where

import Data.Time (UTCTime)

import DomainDrivenDesign.MTL 

import ItHappenedAgain.Tracker.Data 

create
    :: (Monad m, AggregateMonad Tracking Event Error m)
    => TrackingId
    -> String
    -> m ()
create trackerId trackerName = 
    ensureNew >> raiseEvent (Created trackerId trackerName)

track
    :: (Monad m, AggregateMonad Tracking Event Error m)
    => UTCTime 
    -> Maybe GeoCords
    -> m ()
track eventTime eventPlace =
    ensureRunning >> raiseEvent (Happened eventTime eventPlace)

finish 
    :: (Monad m, AggregateMonad Tracking Event Error m)
    => UTCTime 
    -> m ()
finish finishTime =
    ensureRunning >> raiseEvent (Finished finishTime)

ensureNew 
    :: (Monad m, AggregateMonad Tracking Event Error m)
    => m ()
ensureNew =
    getAggregate >>= \case
        Empty -> return ()
        _ -> raiseError TrackingAlreadyExists

ensureRunning
    :: (Monad m, AggregateMonad Tracking Event Error m)
    => m ()
ensureRunning =
    getAggregate >>= \case
        Running _ -> return ()
        Archived _ -> raiseError TrackingAlreadyClosed
        Empty -> raiseError TrackingNotFound
