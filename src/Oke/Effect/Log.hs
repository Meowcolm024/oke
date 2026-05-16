module Oke.Effect.Log (Log, runLog, logDbg, logInfo, logWarn, logErr) where

import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
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
