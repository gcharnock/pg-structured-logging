
module Logging.Contextual.BasicScheme(
   headline,
   headlineQ,
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
import Language.Haskell.TH.Quote
import Text.InterpolatedString.Perl6

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

mkLevelQ :: T.Text -> QuasiQuoter
mkLevelQ level = QuasiQuoter { quoteExp = quoter }
   where textExpQ = quoteExp qq
         quoter str = do
            let msgExp = textExpQ str
            loc <- location
            let filename = loc_filename loc
                (line, col) = loc_start loc
            [|\logger ->
               postRawLog logger LogMsg 
                  { logMsgLevel= $(lift $ T.unpack level)
                  , logMsgBody= $(msgExp)
                  , logMsgData=Nothing 
                  , logMsgFilename=Just filename
                  , logMsgLine=Just line
                  , logMsgCol=Just col
                  }|]

headlineQ :: QuasiQuoter
headlineQ = mkLevelQ "HEADLINE"

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