module Test.DomainDrivenDesign.Silly
    ( given
    , expectSingleEvent
    , expectFailure
    ) where

import DomainDrivenDesign.Silly

expectFailure :: (EventSourced st cmd ev err, Eq ev, Eq err)
              => st
              -> cmd
              -> err
              -> Bool
expectFailure st cmd err = execute st cmd == failure err

expectSingleEvent :: (EventSourced st cmd ev err, Eq ev, Eq err)
            => st
            -> cmd
            -> ev
            -> Bool
expectSingleEvent st cmd ev = execute st cmd == singleEvent ev

given :: (EventSourced st cmd ev err) => [ev] -> st
given = rebuildAggregate
