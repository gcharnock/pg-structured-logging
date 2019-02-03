
module Logging.Contextual.BasicScheme where

import Logging.Contextual
import Data.Text as T
import Data.Aeson
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

headlineRaw :: Logger -> T.Text -> IO ()
headlineRaw logger msg = postRawLog logger $ LogMsg "HEADLINE" msg Nothing

headline :: Q Exp
headline = do
   loc <- location
   let locStr = show loc
   let printExp = [|putStrLn $(lift locStr)|]
   [|\logger msg -> $(printExp) >> headlineRaw logger msg|]

error :: Logger -> T.Text -> IO ()
error logger msg = postRawLog logger $ LogMsg "ERROR" msg Nothing

warning :: Logger -> T.Text -> IO ()
warning logger msg = postRawLog logger $ LogMsg "WARN" msg Nothing

info :: Logger -> T.Text -> IO ()
info logger msg = postRawLog logger $ LogMsg "INFO" msg Nothing

trace :: Logger -> T.Text -> IO ()
trace logger msg = postRawLog logger $ LogMsg "TRACE" msg Nothing

debug :: Logger -> T.Text -> IO ()
debug logger msg = postRawLog logger $ LogMsg "DEBUG" msg Nothing

