module Main where


import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock
import Logging.Contextual
import Logging.Contextual.BasicScheme


producer :: Logger -> Int -> IO NominalDiffTime
producer logger msgCount = do
  now <- liftIO $ getCurrentTime
  withEvent logger (LogEvent "example event" Nothing) $ \logger ->
    replicateM_ msgCount $ $trace logger "example message"
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
  $headline logger "Logger created"
  now <- liftIO getCurrentTime
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