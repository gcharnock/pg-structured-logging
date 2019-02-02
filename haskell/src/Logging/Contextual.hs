
module Logging.Contextual(
    Logger,
    makeLogger,
    withEvent,
    postLog,
    closeLogger
) where

import           Hasql.Connection
import qualified Hasql.Pool                    as Pool
import           Data.Function
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent.Chan
import           Control.Concurrent.Async
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

data Message = Message
    { msgBody :: T.Text
    , msgLevel :: T.Text
    , msgEventId :: Maybe UUID
    , msgTimestamp :: UTCTime
    , msgData :: Maybe Value
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
  where sqlStmnt = "INSERT INTO message(message, level, event_id, timestamp, data) VALUES($1, $2, $3, $4, $5)"
        encoder = contramap msgBody (En.param En.text) <>
                  contramap msgLevel (En.param En.text) <>
                  contramap msgEventId (En.nullableParam  En.uuid) <>
                  contramap msgTimestamp (En.param En.timestamptz) <>
                  contramap msgData (En.nullableParam En.jsonb) 

data LogEvent = LEStart StartEvent | LEEnd FinishEvent | LEMessage Message

data Logger = Logger 
  { lgChan :: Chan (Maybe LogEvent)
  , lgWriters :: [Async (Either Pool.UsageError ())]
  , lgPool :: Pool.Pool
  }

consumer :: Chan (Maybe LogEvent) -> Session () 
consumer chan = fix $ \continue ->
  (liftIO $ readChan chan) >>= \case
    Nothing -> return ()
    Just (LEStart eventStart) -> statement eventStart insertEvent >> continue
    Just (LEEnd eventEnd) -> statement eventEnd finishEvent >> continue
    Just (LEMessage msg) -> statement msg insertMessage >> continue

makeLogger :: Int -> IO Logger
makeLogger writerCount = do
    let poolSettings = (writerCount, 10.0, settings "docker" 30000 "postgres" "dev" "postgres")
    pool <- Pool.acquire poolSettings 
    
    lgChan <- newChan
    consumers <- replicateM writerCount $ async $ Pool.use pool $ consumer lgChan
    return Logger {lgChan, lgWriters = consumers, lgPool = pool} 

closeLogger :: Logger -> IO ()
closeLogger Logger {lgChan, lgWriters, lgPool} = do
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

withEvent :: Logger -> T.Text -> Maybe Value -> IO a -> IO a
withEvent Logger {lgChan} eventType seData action = do
  eventId <- nextRandom
  now <- getCurrentTime
  let event = StartEvent
                { seEventId = eventId
                , seTimestampStart = now 
                , seParent = Nothing
                , seEventType = eventType
                , seData
                }

  writeChan lgChan $ Just $ LEStart event
  result <- action
  end <- getCurrentTime
  writeChan lgChan $ Just $ LEEnd FinishEvent 
               { feEventId = eventId
               , feTimestampEnd = end
               , feError = Nothing
               }
  return result 

postLog :: Logger -> T.Text -> T.Text -> Maybe Value -> IO ()
postLog Logger {lgChan} level message msgData = do
    now <- getCurrentTime
    writeChan lgChan $ Just $ LEMessage Message
       { msgBody = message
       , msgLevel = level
       , msgTimestamp = now
       , msgData
       , msgEventId = Nothing
       } 
