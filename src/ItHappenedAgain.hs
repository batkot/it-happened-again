{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ItHappenedAgain where

import DomainDrivenDesign.MTL

import ItHappenedAgain.Tracker.Data
import ItHappenedAgain.Tracker.MTL

import Data.Text (Text)

data CreateTracking = CreateTracking
    { ctId :: !Int 
    , ctName :: !Text
    }

type TrackingAction m = AggregateActionT Tracking Event Error m
instance Monad m => AggregateMonad Tracking Event Error (TrackingAction m)

createTracking :: Monad m => (Int -> m [Event])-> CreateTracking -> m ()
createTracking fetchEvents CreateTracking{..} = do
    fetchEvents ctId >>= \e -> runAggregate e action >>= storeEvents >>= monadVoid
  where
    monadVoid = const $ return ()
    storeEvents = const $ return True
    trackId = TrackingId ctId
    action = create trackId ctName 
