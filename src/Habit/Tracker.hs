module Habit.Tracker 
    (
    ) where

import qualified DomainDrivenDesign as DDD

import Data.Time (UTCTime)

newtype TrackingId = TrackingId { unTrackingId :: Int } deriving (Show)
newtype GeoCords = GeoCords { unGeoCords :: (Int, Int) } deriving (Show)

data Tracking 
    = Empty
    | Running TrackingData
    | Finished TrackingData

data TrackingData = TrackingData
    { identifier :: !TrackingId
    , name :: !String
    , occurances :: [Occurance]
    } deriving (Show)

data Occurance = Occurance
    { time :: UTCTime
    , place :: Maybe GeoCords
    } deriving (Show)

data Command 
    = Create !TrackingId !String
    | Track !UTCTime (Maybe GeoCords)

data Event
    = Created !TrackingId !String
    | Happened !UTCTime (Maybe GeoCords)

data Error = Error

instance DDD.EventSourced Tracking Command Event Error where
    initState :: Tracking
    initState = Empty

    execute :: Tracking -> Command -> Either Error [Event]
    execute Empty (Create id name) = pure . pure $ Created id name
    execute (Running track) (Track time place) = pure . pure $ Happened time place
    execute _ _ = Left Error

    apply :: Event -> Tracking -> Tracking
    apply _ = id
