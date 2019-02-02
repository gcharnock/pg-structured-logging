
module Logging.Contextual where

import Hasql.Statement
import Hasql.Session
import qualified Data.Text as T
import qualified Hasql.Decoders as De
import qualified Hasql.Encoders as En
import Data.Int
import Data.Time.Clock
import Data.Functor.Contravariant

data Event = Event 
    { timestampStart :: UTCTime
    , parent :: Maybe Int64
    , eventType :: T.Text 
    }

insertEvent :: Statement Event ()
insertEvent = Statement sqlStmnt encoder De.unit True
  where sqlStmnt = "INSERT INTO event(timestamp_start, parent, event_type) VALUES($1, $2, $3)"
        encoder = contramap timestampStart (En.param En.timestamptz) <>
                  contramap parent (En.nullableParam En.int8) <>
                  contramap eventType (En.param En.text)


