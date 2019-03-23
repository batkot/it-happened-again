module ItHappenedAgain.Tracker.Data where

import DomainDrivenDesign.EventSourcing

import Data.Time (UTCTime)

newtype TrackingId = TrackingId { unTrackingId :: Int } deriving (Show, Eq)
newtype GeoCords = GeoCords { unGeoCords :: (Int, Int) } deriving (Show, Eq)

data Tracking 
    = Empty
    | Running TrackingData
    | Archived TrackingData

data TrackingData = TrackingData
    { identifier :: !TrackingId
    , name :: !String
    , occurances :: [Occurance]
    } deriving (Show, Eq)

data Occurance = Occurance
    { time :: UTCTime
    , place :: Maybe GeoCords
    } deriving (Show, Eq)

data Command 
    = Create !TrackingId !String
    | Track !UTCTime (Maybe GeoCords)
    | Finish !UTCTime
    deriving (Show, Eq)

data Event
    = Created !TrackingId !String
    | Happened !UTCTime (Maybe GeoCords)
    | Finished !UTCTime
    deriving (Show, Eq)

data Error 
    = Error
    | TrackingAlreadyClosed
    deriving (Show, Eq)

instance EventSourced Tracking Event where
    initState = Empty
    
    apply (Created trackId trackName) Empty = 
        Running $ TrackingData trackId trackName []
    apply (Happened evTime evPlace) (Running track) = 
        Running $ track { occurances = newOccurances }
      where
        occurance = Occurance evTime evPlace
        newOccurances = occurance : occurances track
    apply (Finished _) (Running track) = Archived track
    apply _ s = s
