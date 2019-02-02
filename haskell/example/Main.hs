module Main where

import Hasql.Session
import Hasql.Connection
import qualified Hasql.Pool as Pool

import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock
import Logging.Contextual
import Control.Concurrent.Async
import Control.Concurrent.Chan


producer :: Logger -> Int -> IO NominalDiffTime
producer logger size = do
  now <- liftIO $ getCurrentTime
  replicateM_ size $ withEvent logger "example event" $ return ()
  end <- liftIO $ getCurrentTime
  return $ end `diffUTCTime` now

runTest :: Int -> IO ()
runTest size = do
  logger <- makeLogger
  let poolSettings = (size, 10.0, settings "docker" 30000 "postgres" "dev" "postgres")
  pool <- Pool.acquire poolSettings 

  producerTask <- async $ producer logger (size * 10000)

  now <- liftIO $ getCurrentTime
  consumers <- replicateM size $ async $ Pool.use pool $ consumer logger

  producerTime <- wait producerTask
  putStrLn $ show $ fromIntegral size * (10000.0 :: Double) / realToFrac producerTime 

  replicateM_ size $ writeChan (lgChan logger) Nothing
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