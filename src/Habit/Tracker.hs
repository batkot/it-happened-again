module Habit.Tracker 
    ( Tracking
    , TrackingId(..)
    , GeoCords(..)
    , Command(..)
    , Event(..)
    , Error(..)
    ) where

import qualified DomainDrivenDesign.Silly as DDD

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

innerApply :: Event -> Tracking -> Tracking
innerApply (Created trackId trackName) Empty = 
    Running $ TrackingData trackId trackName []
innerApply (Happened evTime evPlace) (Running track) = 
    Running $ track { occurances = newOccurances }
  where
    occurance = Occurance evTime evPlace
    newOccurances = occurance : occurances track
innerApply (Finished _) (Running track) = Archived track
innerApply _ s = s

instance DDD.EventSourced Tracking Command Event Error where
    initState :: Tracking
    initState = Empty

    execute :: Tracking -> Command -> Either Error [Event]
    execute Empty (Create trackId trackName) = DDD.singleEvent $ Created trackId trackName
    execute (Running _) (Track evTime evPlace) = DDD.singleEvent $ Happened evTime evPlace
    execute (Running _) (Finish finishTime) = DDD.singleEvent $ Finished finishTime
    execute (Archived _) _ = DDD.failure TrackingAlreadyClosed
    execute _ _ = DDD.failure Error

    apply :: Event -> DDD.Restoring Tracking -> DDD.Restoring Tracking
    apply ev = fmap $ innerApply ev
