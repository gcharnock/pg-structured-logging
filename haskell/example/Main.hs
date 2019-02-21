module Main where

import qualified Data.Text as T
import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock
import Control.Monad.Trans.Reader(runReaderT, ReaderT, ask)


import Logging.Contextual
import Logging.Contextual.BasicScheme 

producer :: Int -> ReaderT Logger IO NominalDiffTime
producer msgCount = do
  logger <- ask
  now <- liftIO $ getCurrentTime
  withEventM (LogEvent "example event" Nothing) $ do
    logger' <- ask
    replicateM_ msgCount $ [logTrace|example message|]
  end <- liftIO $ getCurrentTime
  return $ end `diffUTCTime` now

runTest :: Int -> IO ()
runTest size = do
  let logSettings = LoggerSettings 
         { lsWriterCount = size
         , lsHostname = "localhost"
         , lsPort = 5432
         , lsUsername = "postgres"
         , lsPassword = ""
         , lsDbName = "log"
         }

  logger <- makeLogger logSettings (LogEvent "app startup" Nothing)
  flip runReaderT logger $ do
   
    [logHeadline|log headline|] 
    [logError|log error|] 
    [logWarning|log warning|] 
    [logInfo|log info|] 
    [logTrace|log trace|] 
    
    
    now <- liftIO getCurrentTime
    
    producerTime <- producer (size * 10000)

    let bustRate = fromIntegral size * (10000.0 :: Double) / realToFrac producerTime
    [logHeadline|burst rate={bustRate} lines/sec|]

    liftIO $ closeLogger logger
    consumerTime <- liftIO $ getCurrentTime
    let dbRate = fromIntegral size * (10000.0 :: Double) / realToFrac (consumerTime `diffUTCTime` now) 
    liftIO $ putStrLn $ "db rate=" <> show dbRate <> "lines/sec"


main :: IO ()
main = do
  runTest 1
  runTest 2
  runTest 3
  runTest 4