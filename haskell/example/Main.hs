module Main where

import Hasql.Session
import Hasql.Connection
import qualified Hasql.Pool as Pool

import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock
import NestedLogger
import Control.Concurrent.Async

exampleSession :: Session ()
exampleSession = do
  now <- liftIO $ getCurrentTime
  let event = Event { timestampStart = now 
                    , parent = Nothing
                    , eventType = "ExampleApp"
                    }
  replicateM_ 10000 $ statement event insertEvent
  return ()

runTest :: Int -> IO ()
runTest size = do
  let poolSettings = (size, 10.0, settings "docker" 30000 "postgres" "dev" "postgres")
  pool <- Pool.acquire poolSettings 

  now <- liftIO $ getCurrentTime
  s <- replicateM size $ async $ Pool.use pool exampleSession 
  results <- mapM wait s
  end <- liftIO $ getCurrentTime
  let deltaT = end `diffUTCTime` now

  putStrLn $ show $ fromIntegral size * 10000.0 / realToFrac deltaT

  forM_ results $ \result -> do
    case result of
      Right _ -> return ()
      Left qe -> print qe

  Pool.release pool

main :: IO ()
main = do
  runTest 1
  runTest 2
  runTest 3
  runTest 4
  runTest 5