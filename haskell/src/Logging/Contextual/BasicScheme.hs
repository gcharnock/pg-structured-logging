
module Logging.Contextual.BasicScheme where

import Logging.Contextual
import Data.Text as T
import Data.Aeson
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

headlineRaw :: Logger -> T.Text -> IO ()
headlineRaw logger msg = postRawLog logger $ LogMsg "HEADLINE" msg Nothing Nothing

headline :: Q Exp
headline = do
   loc <- location
   let locExp = [|Loc $(lift $ loc_filename loc)
                   $(lift $ loc_package loc)
                   $(lift $ loc_module loc)
                   $(lift $ loc_start loc)
                   $(lift $ loc_end loc)
             |]
   [|\logger msg -> postRawLog logger $ LogMsg "HEADLINE" msg Nothing (Just $(locExp))|]

error :: Logger -> T.Text -> IO ()
error logger msg = postRawLog logger $ LogMsg "ERROR" msg Nothing Nothing

warning :: Logger -> T.Text -> IO ()
warning logger msg = postRawLog logger $ LogMsg "WARN" msg Nothing Nothing

info :: Logger -> T.Text -> IO ()
info logger msg = postRawLog logger $ LogMsg "INFO" msg Nothing Nothing

trace :: Logger -> T.Text -> IO ()
trace logger msg = postRawLog logger $ LogMsg "TRACE" msg Nothing Nothing

debug :: Logger -> T.Text -> IO ()
debug logger msg = postRawLog logger $ LogMsg "DEBUG" msg Nothing Nothing

