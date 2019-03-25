{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.ItHappenedAgain.Tracker.Arbitrary where

import Test.QuickCheck (Arbitrary, arbitrary, listOf, oneof, Gen)

import qualified Data.Time as DT
import qualified Data.Time.Calendar as DTC
import qualified ItHappenedAgain.Tracker.Data as HT

import Data.List.NonEmpty

instance Arbitrary DT.UTCTime where
    arbitrary = DT.UTCTime <$> dayGen <*> diffTimeGen
      where
        dayGen = DTC.fromGregorian <$> arbitrary <*> arbitrary <*> arbitrary
        diffTimeGen = DT.secondsToDiffTime <$> arbitrary

instance Arbitrary HT.TrackingId where
    arbitrary = HT.TrackingId <$> arbitrary

instance Arbitrary HT.GeoCords where
    arbitrary = HT.GeoCords <$> arbitrary

newtype EventsForRunningTracking = EventsForRunningTracking { runningEvents :: [HT.Event] }
    deriving Show

instance Arbitrary EventsForRunningTracking where
    arbitrary = do
        created <- HT.Created <$> arbitrary <*> arbitrary
        list <- listOf (HT.Happened <$> arbitrary <*> arbitrary)
        return $ EventsForRunningTracking (list ++ [created])

newtype ClosedTrackingEvents = ClosedTrackingEvents { closedEvents :: [HT.Event] }
    deriving Show

instance Arbitrary ClosedTrackingEvents where
    arbitrary = do 
        running <- arbitrary :: Gen EventsForRunningTracking
        closed <- HT.Finished <$> arbitrary
        return $ ClosedTrackingEvents $ closed : (runningEvents running)
        

instance Arbitrary HT.Event where
    arbitrary = oneof
        [ HT.Created <$> arbitrary <*> arbitrary
        , HT.Happened <$> arbitrary <*> arbitrary
        , HT.Finished <$> arbitrary 
        ]

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
