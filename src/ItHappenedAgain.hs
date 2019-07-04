{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ItHappenedAgain where

import DomainDrivenDesign.MTL

import ItHappenedAgain.Tracker

import Data.Text (Text)

data CreateTracking = CreateTracking
    { ctId :: !Int 
    , ctName :: !Text
    }

-- | This thing should build some kind of event processor
createTracking 
    :: forall m. Monad m 
    => (Int -> m [Event]) 
    -> CreateTracking 
    -> m ()
createTracking fetchEvents CreateTracking{..} = 
    fetchEvents ctId >>= runAggregateActionT action >>= storeEvents 
  where
    storeEvents = const $ return ()
    trackId = TrackingId ctId
    action :: AggregateActionT Tracking Event Error m ()
    action = create trackId ctName 

