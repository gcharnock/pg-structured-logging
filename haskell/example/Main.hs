module Main where

import qualified Data.Text as T
import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock
import Logging.Contextual
import Logging.Contextual.BasicScheme
import NeatInterpolation
import Control.Monad.Trans.Reader(runReaderT, ReaderT, ask)

producer :: Int -> ReaderT Logger IO NominalDiffTime
producer msgCount = do
  logger <- ask
  now <- liftIO $ getCurrentTime
  withEventM (LogEvent "example event" Nothing) $ do
    logger' <- ask
    replicateM_ msgCount $ $trace logger' "example message"
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
    let t = "world" :: T.Text
    let s = [text|hello ${t}|]
    $headline logger s
    [headlineQ|logger created. Hostname = {t <> "foobar"}|] 
    now <- liftIO getCurrentTime
    
    producerTime <- producer (size * 10000)

    let bustRate = fromIntegral size * (10000.0 :: Double) / realToFrac producerTime
    [headlineQ|burst rate={bustRate} lines/sec|]

    consumerTime <- liftIO $ getCurrentTime
    let dbRate = fromIntegral size * (10000.0 :: Double) / realToFrac (consumerTime `diffUTCTime` now) 
    [headlineQ|db rate={dbRate} lines/sec|]

  closeLogger logger

main :: IO ()
main = do
  runTest 1
  runTest 2
  runTest 3
  runTest 4