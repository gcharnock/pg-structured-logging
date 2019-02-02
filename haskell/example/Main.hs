module Main where

import Hasql.Session
import Hasql.Connection
import qualified Hasql.Pool as Pool

import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock
import NestedLogger
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Data.Function

consumer :: Chan (Maybe Event) -> Session () 
consumer chan = fix $ \continue ->
  (liftIO $ readChan chan) >>= \case
    Nothing -> return ()
    Just event -> statement event insertEvent >> continue

producer :: Int -> Chan (Maybe Event) -> IO NominalDiffTime
producer size chan = do
  now <- liftIO $ getCurrentTime
  let event = Event { timestampStart = now 
                    , parent = Nothing
                    , eventType = "ExampleApp"
                    }

  replicateM_ size $ writeChan chan (Just event)
  end <- liftIO $ getCurrentTime
  return $ end `diffUTCTime` now

runTest :: Int -> IO ()
runTest size = do
  chan <- newChan
  let poolSettings = (size, 10.0, settings "docker" 30000 "postgres" "dev" "postgres")
  pool <- Pool.acquire poolSettings 

  producerTask <- async $ producer (size * 10000) chan

  now <- liftIO $ getCurrentTime
  consumers <- replicateM size $ async $ Pool.use pool $ consumer chan 

  producerTime <- wait producerTask
  putStrLn $ show $ fromIntegral size * (10000.0 :: Double) / realToFrac producerTime 

  replicateM_ size $ writeChan chan Nothing
  mapM_ wait consumers

  consumerTime <- getCurrentTime
  putStrLn $ show $ fromIntegral size * (10000.0 :: Double) / realToFrac (consumerTime `diffUTCTime` now) 
  
  Pool.release pool
  putStrLn ""

main :: IO ()
main = do
  runTest 1
  runTest 2
  runTest 3
  runTest 4