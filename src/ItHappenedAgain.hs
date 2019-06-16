{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ItHappenedAgain where

import DomainDrivenDesign.MTL

import ItHappenedAgain.Tracker.Data
import ItHappenedAgain.Tracker.Tagless

import Data.Text (Text)

data CreateTracking = CreateTracking
    { ctId :: !Int 
    , ctName :: !Text
    }

createTracking :: forall m. Monad m => (Int -> m [Event])-> CreateTracking -> m ()
createTracking fetchEvents CreateTracking{..} = do
    fetchEvents ctId >>= \e -> runAggregateActionT e action >>= storeEvents 
  where
    storeEvents = const $ return ()
    trackId = TrackingId ctId
    action :: AggregateActionT Tracking Event Error m ()
    action = create trackId ctName 
