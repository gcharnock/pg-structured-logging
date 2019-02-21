module Logging.Contextual(
    Logger,
    LoggerSettings(..),
    LogMsg(..),
    LogEvent(..),
    makeLogger,
    withEvent,
    postRawLog,
    closeLogger,
    withLogger,
    HasLog(..),
    postRawLogM,
    withEventM
) where

import Control.Exception
import           Hasql.Connection
import qualified Hasql.Pool                    as Pool
import           Data.Function
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent.Chan
import           Control.Concurrent.Async
import qualified Data.ByteString               as BS
import           Hasql.Statement
import           Hasql.Session
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Hasql.Decoders                as De
import qualified Hasql.Encoders                as En
import           Data.Time.Clock
import           Data.Functor.Contravariant
import           Data.Aeson
import           Data.UUID
import           Data.UUID.V4
import           Data.Word
import Control.Monad.Reader.Class
import Control.Monad.IO.Unlift

data StartEvent = StartEvent 
    { seEventId :: UUID
    , seTimestampStart :: UTCTime
    , seParent :: Maybe UUID
    , seEventType :: T.Text 
    , seData :: Maybe Value
    }

data FinishEvent = FinishEvent
    { feEventId :: UUID
    , feTimestampEnd :: UTCTime
    , feError :: Maybe Value
    } 

data LogMsg = LogMsg
    { logMsgLevel :: T.Text
    , logMsgBody :: T.Text
    , logMsgData :: Maybe Value
    , logMsgFilename :: Maybe T.Text
    , logMsgLine :: Maybe Int
    , logMsgCol :: Maybe Int
    } 

data Message = Message
    { msgEventId :: Maybe UUID
    , msgTimestamp :: UTCTime
    , msgData :: LogMsg 
    } 


data LogEvent = LogEvent
    { logEvType :: T.Text
    , logEvData :: Maybe Value
    } 

insertEvent :: Statement StartEvent () 
insertEvent = Statement sqlStmnt encoder De.unit True
  where sqlStmnt = "INSERT INTO event(event_id, timestamp_start, parent, event_type) VALUES($1, $2, $3, $4)"
        encoder = contramap seEventId (En.param En.uuid) <>
                  contramap seTimestampStart (En.param En.timestamptz) <>
                  contramap seParent (En.nullableParam En.uuid) <>
                  contramap seEventType (En.param En.text)

finishEvent :: Statement FinishEvent ()
finishEvent = Statement sqlStmnt encoder De.unit True
  where sqlStmnt = "UPDATE event SET timestamp_end=$1, error=$2 WHERE event_id=$3"
        encoder = contramap feTimestampEnd (En.param En.timestamptz) <>
                  contramap feError (En.nullableParam En.jsonb) <>
                  contramap feEventId (En.param En.uuid)

insertMessage :: Statement Message ()
insertMessage = Statement sqlStmnt encoder De.unit True
  where sqlStmnt = "INSERT INTO message(message, level, event_id, timestamp, data, filename, line, col) VALUES($1, $2, $3, $4, $5, $6, $7, $8)"
        encoder = contramap (logMsgBody.msgData) (En.param En.text) <>
                  contramap (logMsgLevel.msgData) (En.param En.text) <>
                  contramap msgEventId (En.nullableParam  En.uuid) <>
                  contramap msgTimestamp (En.param En.timestamptz) <>
                  contramap (logMsgData.msgData) (En.nullableParam En.jsonb) <>
                  contramap (logMsgFilename.msgData) (En.nullableParam En.text) <>
                  contramap (fmap fromIntegral.logMsgLine.msgData) (En.nullableParam En.int4) <>
                  contramap (fmap fromIntegral.logMsgCol.msgData) (En.nullableParam En.int4) 

data ChanMsg = LEStart StartEvent | LEEnd FinishEvent | LEMessage Message

data Logger = Logger 
  { lgChan :: Chan (Maybe ChanMsg)
  , lgWriters :: [Async (Either Pool.UsageError ())]
  , lgPool :: Pool.Pool
  , lgEventId :: UUID
  , lgParentId :: Maybe UUID
  } 

data LoggerSettings = LoggerSettings
  { lsWriterCount :: Int
  , lsHostname :: BS.ByteString
  , lsPort :: Word16
  , lsUsername :: BS.ByteString
  , lsPassword :: BS.ByteString
  , lsDbName :: BS.ByteString
  }

consumer :: Chan (Maybe ChanMsg) -> Session () 
consumer chan = fix $ \continue ->
  (liftIO $ readChan chan) >>= \case
    Nothing -> return ()
    Just (LEStart eventStart) -> statement eventStart insertEvent >> continue
    Just (LEEnd eventEnd) -> statement eventEnd finishEvent >> continue
    Just (LEMessage msg) -> statement msg insertMessage >> continue

makeLogger :: LoggerSettings -> LogEvent -> IO Logger
makeLogger LoggerSettings {lsWriterCount, lsHostname, lsPort, lsUsername, lsPassword, lsDbName} logEvent = do
    let pgSettings = settings lsHostname lsPort lsUsername lsPassword lsDbName
    let poolSettings = (lsWriterCount, 10.0, pgSettings)
    pool <- Pool.acquire poolSettings 
    
    lgChan <- newChan
    consumers <- replicateM lsWriterCount $ async $ Pool.use pool $ consumer lgChan
    lgEventId <- beginEvent lgChan logEvent
    return $ Logger {lgChan, lgWriters = consumers, lgPool = pool, lgEventId, lgParentId = Nothing}

withLogger :: LoggerSettings -> LogEvent -> (Logger -> IO a) -> IO a
withLogger loggerSettings logEvent = bracket (makeLogger loggerSettings logEvent) closeLogger

closeLogger :: Logger -> IO ()
closeLogger Logger {lgChan, lgWriters, lgPool, lgEventId} = do
  endEvent lgChan lgEventId
  replicateM_ (length lgWriters) $ writeChan lgChan Nothing
  results <- mapM wait lgWriters :: IO [Either Pool.UsageError ()]

  forM_ results $ \case
    Right () -> return ()
    Left usageError -> do
        T.putStrLn "======= Logging Error ========" 
        T.putStrLn "Usage error from writer thread" 
        print usageError
        T.putStrLn "======= End Error ========" 

  Pool.release lgPool

beginEvent :: Chan (Maybe ChanMsg) -> LogEvent -> IO UUID
beginEvent chan LogEvent {logEvType, logEvData} = do
            eventId <- nextRandom
            now <- getCurrentTime
            let event = StartEvent
                          { seEventId = eventId
                          , seTimestampStart = now 
                          , seParent = Nothing
                          , seEventType = logEvType
                          , seData = logEvData
                          }
            writeChan chan $ Just $ LEStart event
            return eventId

endEvent :: Chan (Maybe ChanMsg) -> UUID -> IO ()
endEvent chan feEventId = do
            endTime <- getCurrentTime
            writeChan chan $ Just $ LEEnd FinishEvent 
                         { feEventId 
                         , feTimestampEnd = endTime
                         , feError = Nothing
                         }
withEvent :: Logger -> LogEvent -> (Logger -> IO a) -> IO a
withEvent logger@Logger {lgChan} logEvent action = 
    bracket (beginEvent lgChan logEvent) (endEvent lgChan) $ \lgEventId ->
        action $ logger {lgEventId}

postRawLog :: Logger -> LogMsg -> IO ()
postRawLog Logger {lgChan, lgEventId} logMsg@LogMsg {logMsgLevel, logMsgBody, logMsgData} = do
    now <- getCurrentTime
    writeChan lgChan $ Just $ LEMessage Message
       { msgData = logMsg 
       , msgTimestamp = now
       , msgEventId = Just lgEventId
       } 

class HasLog a where
   getLog :: a -> Logger
   setLog :: Logger -> a -> a
instance HasLog Logger where
   getLog = id
   setLog = const

postRawLogM :: (MonadReader env m, MonadIO m, HasLog env) => LogMsg -> m ()
postRawLogM msg = do
    logger <- getLog <$> ask
    liftIO $ postRawLog logger msg

withEventM :: (MonadUnliftIO m, MonadReader env m, MonadIO m, HasLog env) => LogEvent -> m a -> m a
withEventM logEvent action = do
    logger <- getLog <$> ask
    runInIO <- askRunInIO
    liftIO $ withEvent logger logEvent $ \logger' -> runInIO $ local (setLog logger') action
