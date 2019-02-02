
module Logging.Contextual where

import Data.Function
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.Chan
import Hasql.Statement
import Hasql.Session
import qualified Data.Text as T
import qualified Hasql.Decoders as De
import qualified Hasql.Encoders as En
import Data.Int
import Data.Time.Clock
import Data.Functor.Contravariant
import Data.Aeson
import Data.UUID
import Data.UUID.V4

data StartEvent = StartEvent 
    { seEventId :: UUID
    , seTimestampStart :: UTCTime
    , seParent :: Maybe UUID
    , seEventType :: T.Text 
    }

data FinishEvent = FinishEvent
    { feEventId :: UUID
    , feTimestampEnd :: UTCTime
    , feError :: Maybe Value
    }

insertEvent :: Statement StartEvent UUID
insertEvent = Statement sqlStmnt encoder (De.singleRow $ De.column De.uuid) True
  where sqlStmnt = "INSERT INTO event(event_id, timestamp_start, parent, event_type) VALUES($1, $2, $3, $4) RETURNING event_id"
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

data LogEvent = LEStart StartEvent | LEEnd FinishEvent

data Logger = Logger 
  { lgChan :: Chan (Maybe LogEvent)
  }

consumer :: Logger -> Session () 
consumer Logger {lgChan} = fix $ \continue ->
  (liftIO $ readChan lgChan) >>= \case
    Nothing -> return ()
    Just (LEStart eventStart) -> statement eventStart insertEvent >> continue
    Just (LEEnd eventEnd) -> statement eventEnd finishEvent >> continue


makeLogger :: IO Logger
makeLogger = do
    lgChan <- newChan
    return Logger {lgChan} 

withEvent :: Logger -> T.Text -> IO a -> IO a
withEvent Logger {lgChan} eventType action = do
  eventId <- return nil -- nextRandom
  now <- liftIO $ getCurrentTime
  let event = StartEvent
                { seEventId = eventId
                , seTimestampStart = now 
                , seParent = Nothing
                , seEventType = eventType
                }

  writeChan lgChan $ Just $ LEStart event
  result <- action
  end <- liftIO $ getCurrentTime
  writeChan lgChan $ Just $ LEEnd FinishEvent 
               { feEventId = eventId
               , feTimestampEnd = end
               , feError = Nothing
               }
  return result 
