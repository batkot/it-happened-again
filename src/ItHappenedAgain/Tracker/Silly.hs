{-# OPTIONS_GHC -fno-warn-orphans #-}

module ItHappenedAgain.Tracker.Silly
    ( Tracking
    , TrackingId(..)
    , GeoCords(..)
    , Command(..)
    , Event(..)
    , Error(..)
    ) where

import qualified DomainDrivenDesign.Silly as DDD
import qualified DomainDrivenDesign.EventSourcing as ES

import ItHappenedAgain.Tracker.Data

import Data.Time (UTCTime)
import Data.Text (Text)

data Command 
    = Create !TrackingId !Text
    | Track !UTCTime (Maybe GeoCords)
    | Finish !UTCTime
    deriving (Show, Eq)

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
    apply ev = fmap $ ES.apply ev
