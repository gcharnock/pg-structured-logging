module Main where


import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock
import Logging.Contextual


producer :: Logger -> Int -> IO NominalDiffTime
producer logger msgCount = do
  now <- liftIO $ getCurrentTime
  withEvent logger "example event" Nothing $
    replicateM_ msgCount $ postLog logger "TRACE" "example message" Nothing 
  end <- liftIO $ getCurrentTime
  return $ end `diffUTCTime` now

runTest :: Int -> IO ()
runTest size = do
  logger <- makeLogger size

  now <- liftIO $ getCurrentTime
  producerTime <- producer logger (size * 10000)
  print $ fromIntegral size * (10000.0 :: Double) / realToFrac producerTime 

 
  closeLogger logger
  consumerTime <- getCurrentTime
  print $ fromIntegral size * (10000.0 :: Double) / realToFrac (consumerTime `diffUTCTime` now) 

  putStrLn ""

main :: IO ()
main = do
  runTest 1
  runTest 2
  runTest 3
  runTest 4