module Test.DomainDrivenDesign.MTL 
    ( TestTrackerAggregate 
    , given
    ) where

import Control.Monad.Identity

import DomainDrivenDesign.MTL

type TestTrackerAggregate st ev err a = AggregateActionT st ev err Identity

given :: (AggregateMonad st ev err m) => [ev] -> m ()
given = rebuildAggregate
