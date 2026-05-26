module Oke.Effect.Log where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import Path
import Path.IO
import System.IO
import System.Log.Formatter
import System.Log.Handler
import System.Log.Handler.Simple
import System.Log.Logger (Logger)
import System.Log.Logger qualified as L

data Log :: Effect where
  Log :: forall m. L.Priority -> Text -> Log m ()

type instance DispatchOf Log = Dynamic

runLog :: forall es a. (IOE :> es) => L.Logger -> Eff (Log : es) a -> Eff es a
runLog logger = interpret $ \_ (Log prio msg) -> liftIO $ L.logL logger prio (T.unpack msg)

logM :: forall es. (Log :> es) => L.Priority -> Text -> Eff es ()
logM prio msg = send (Log prio msg)

logDbg :: forall es. (Log :> es) => Text -> Eff es ()
logDbg = logM L.DEBUG

logInfo :: forall es. (Log :> es) => Text -> Eff es ()
logInfo = logM L.INFO

logWarn :: forall es. (Log :> es) => Text -> Eff es ()
logWarn = logM L.WARNING

logErr :: forall es. (Log :> es) => Text -> Eff es ()
logErr = logM L.ERROR

setupLogger :: Path Abs File -> IO Logger
setupLogger logPath = do
  chdl <- streamHandler stdout L.INFO
  let chFmt = setFormatter chdl (simpleLogFormatter "[$prio] $msg")
  fhdl <- fileHandler (fromAbsFile logPath) L.DEBUG
  let fhFmt = setFormatter fhdl (simpleLogFormatter "$time [$prio] $msg")
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG . L.setHandlers [chFmt, fhFmt])
  L.getRootLogger

cleanupLogger :: Path Abs File -> IO ()
cleanupLogger logPath = do
  L.removeAllHandlers
  trimLogFile logPath

trimLogFile :: Path Abs File -> IO ()
trimLogFile path = do
  exists <- doesFileExist path
  when exists $ do
    size <- getFileSize path
    when (size > maxBytes) $ do
      withFile (fromAbsFile path) ReadMode $ \h -> do
        hSeek h SeekFromEnd (-keepBytes)
        !recentLog <- B.hGetContents h
        let notice = "[... Log truncated for size ...]\n"
        B.writeFile (fromAbsFile path) (notice <> recentLog)
  where
    maxBytes = 2 * 1024 * 1024
    keepBytes = 512 * 1024
