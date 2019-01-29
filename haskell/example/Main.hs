module Main where

import Hasql.Connection
import Hasql.Session

import Data.Time.Clock
import NestedLogger

exampleSession :: Session ()
exampleSession = do
  let event = Event { timestampStart = undefined
                    , parent = Nothing
                    , eventType = "ExampleApp"
                    }
  statement event insertEvent
  return ()

main :: IO ()
main = do
    Right connection <- acquire $ settings "docker" 30000 "postgres" "dev" "postgres"
    run exampleSession connection
    return ()