
module Logging.Contextual.BasicScheme(
   headline,
   error,
   warning,
   info,
   trace
) where

import Prelude hiding (error)

import Logging.Contextual
import Data.Text as T
import Data.Aeson
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

mkLevel :: T.Text -> Q Exp
mkLevel level = do
   loc <- location
   let filename = loc_filename loc
       (line, col) = loc_start loc
   [|\logger msg ->
      postRawLog logger LogMsg 
         { logMsgLevel= $(lift $ T.unpack level)
         , logMsgBody=msg
         , logMsgData=Nothing 
         , logMsgFilename=Just filename
         , logMsgLine=Just line
         , logMsgCol=Just col
         }|]

headline :: Q Exp
headline = mkLevel "HEADLINE"

error :: Q Exp
error = mkLevel "ERROR"

warning :: Q Exp
warning = mkLevel "WARNING"

info :: Q Exp
info = mkLevel "INFO"

trace :: Q Exp
trace = mkLevel "TRACE"