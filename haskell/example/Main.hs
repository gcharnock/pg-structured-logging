module Main where


import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock
import Logging.Contextual


producer :: Logger -> Int -> IO NominalDiffTime
producer logger msgCount = do
  now <- liftIO $ getCurrentTime
  withEvent logger (LogEvent "example event" Nothing) $ \logger ->
    replicateM_ msgCount $ postRawLog logger (LogMsg "TRACE" "example message" Nothing)
  end <- liftIO $ getCurrentTime
  return $ end `diffUTCTime` now

runTest :: Int -> IO ()
runTest size = do
  let logSettings = LoggerSettings 
         { lsWriterCount = size
         , lsHostname = "docker"
         , lsPort = 30000
         , lsUsername = "postgres"
         , lsPassword = "dev"
         , lsDbName = "postgres"
         }

  logger <- makeLogger logSettings (LogEvent "app startup" Nothing)
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