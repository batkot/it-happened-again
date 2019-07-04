{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ItHappenedAgain 
    ( CreateTracking
    , createTracking
    , createTrackingHandler

    , TrackOccurance
    , trackOccurance

    , FinishTracking
    , finishTracking

    , CommandHandler(..)
    ) where

import DomainDrivenDesign.Aggregate
import ItHappenedAgain.Tracker

import Data.Text (Text)
import Data.Time (UTCTime)

data CreateTracking = CreateTracking
    { ctId :: !Int 
    , ctName :: !Text
    }

data CommandHandler key m = CommandHandler
    { aggregateId :: key
    , action :: m ()
    }

createTrackingHandler 
    :: MonadAggregateAction Tracking Event Error m
    => CreateTracking
    -> CommandHandler Int m
createTrackingHandler CreateTracking{..} =
    CommandHandler ctId $ create trackId ctName 
  where
    trackId = TrackingId ctId

createTracking
    :: MonadAggregateAction Tracking Event Error m
    => CreateTracking
    -> m ()
createTracking CreateTracking{..} =
    create trackId ctName
  where
    trackId = TrackingId ctId

data TrackOccurance = TrackOccurance
    { tId :: !Int
    , tTime :: UTCTime 
    , tGeoCords :: Maybe (Int, Int)
    }

trackOccurance
    :: MonadAggregateAction Tracking Event Error m
    => (Int -> m ()) -- repository ??
    -> TrackOccurance
    -> m ()
trackOccurance fetchAggregate TrackOccurance{..} =
    fetchAggregate tId >> track tTime geoCords
  where
    geoCords = fmap GeoCords tGeoCords

data FinishTracking = FinishTracking
    { fId :: !Int
    , finishTime :: !UTCTime
    }

finishTracking 
    :: MonadAggregateAction Tracking Event Error m
    => (Int -> m ()) -- repository ??
    -> FinishTracking
    -> m ()
finishTracking fetchAggregate FinishTracking{..} =
    fetchAggregate fId >> finish finishTime

-- | This thing should build some kind of event processor
-- createTracking 
--    :: forall m. Monad m 
--    => (Int -> m [Event]) 
--    -> CreateTracking 
--    -> m ()
--createTracking fetchEvents CreateTracking{..} = 
--    fetchEvents ctId >>= runAggregateActionT action >>= storeEvents 
--  where
--    storeEvents = const $ return ()
--    trackId = TrackingId ctId
--    action :: AggregateActionT Tracking Event Error m ()
--    action = create trackId ctName 
