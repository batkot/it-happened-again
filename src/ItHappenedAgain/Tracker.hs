{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module ItHappenedAgain.Tracker 
    ( create
    , track
    , finish

    , Tracking(..)
    , TrackingId(..)
    , GeoCords(..)
    , TrackingData(..)
    , Occurance(..)
    , Event(..)
    , Error(..)
    ) where

import DomainDrivenDesign.EventSourcing
import DomainDrivenDesign.Aggregate

import Data.Time (UTCTime)
import Data.Text (Text)

newtype TrackingId = TrackingId { unTrackingId :: Int } deriving (Show, Eq)
newtype GeoCords = GeoCords { unGeoCords :: (Int, Int) } deriving (Show, Eq)

data Tracking 
    = Empty
    | Running TrackingData
    | Archived TrackingData

data TrackingData = TrackingData
    { identifier :: !TrackingId
    , name :: !Text
    , occurances :: [Occurance]
    } deriving (Show, Eq)

data Occurance = Occurance
    { time :: UTCTime
    , place :: Maybe GeoCords
    } deriving (Show, Eq)

data Event
    = Created !TrackingId !Text
    | Happened !UTCTime (Maybe GeoCords)
    | Finished !UTCTime
    deriving (Show, Eq)

data Error 
    = Error
    | TrackingAlreadyExists
    | TrackingNotFound
    | TrackingAlreadyClosed
    deriving (Show, Eq)

instance EventSourced Tracking Event where
    initState = Empty
    
    apply (Created trackId trackName) Empty = 
        Running $ TrackingData trackId trackName []
    apply (Happened evTime evPlace) (Running trackData) = 
        Running $ trackData { occurances = newOccurances }
      where
        occurance = Occurance evTime evPlace
        newOccurances = occurance : occurances trackData
    apply (Finished _) (Running trackData) = Archived trackData
    apply _ s = s 

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
