{-# LANGUAGE MultiWayIf #-}
module Logging.Contextual.BasicScheme(
   logHeadline,
   logError,
   logWarning,
   logInfo,
   logTrace
) where

import Prelude hiding (error)

import Rainbow
import Logging.Contextual
import Data.Text as T
import Data.Aeson
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Text.InterpolatedString.Perl6

mkLevel :: T.Text -> QuasiQuoter
mkLevel level = QuasiQuoter { quoteExp = quoter }
   where textExpQ = quoteExp qq
         quoter str = do
            let msgExp = textExpQ str
            loc <- location
            let filename = loc_filename loc
                (line, col) = loc_start loc
            let postExp = [|postRawLogM LogMsg 
                                { logMsgLevel= $(lift $ T.unpack level)
                                , logMsgBody= $(msgExp)
                                , logMsgData=Nothing 
                                , logMsgFilename=Just filename
                                , logMsgLine=Just line
                                , logMsgCol=Just col
                                }|]
            if | level == "HEADLINE" -> [|(liftIO $ putChunkLn $ (chunk :: T.Text -> Chunk T.Text) $(msgExp) & fore blue) >> $(postExp) |]
               | level == "ERROR" ->    [|(liftIO $ putChunkLn $ (chunk :: T.Text -> Chunk T.Text) $(msgExp) & fore red) >> $(postExp) |]
               | level == "WARNING" ->  [|(liftIO $ putChunkLn $ (chunk :: T.Text -> Chunk T.Text) $(msgExp) & fore yellow) >> $(postExp) |]
               | level == "INFO" ->     [|(liftIO $ putChunkLn $ (chunk :: T.Text -> Chunk T.Text) $(msgExp) & fore grey) >> $(postExp) |]
               | otherwise -> postExp

logHeadline :: QuasiQuoter
logHeadline =  mkLevel "HEADLINE"

logError :: QuasiQuoter
logError = mkLevel "ERROR"

logWarning :: QuasiQuoter
logWarning = mkLevel "WARNING"

logInfo :: QuasiQuoter
logInfo = mkLevel "INFO"

logTrace :: QuasiQuoter
logTrace = mkLevel "TRACE"