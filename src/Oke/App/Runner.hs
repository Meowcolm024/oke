module Oke.App.Runner where

import Control.Exception (bracket)
import Effectful (runEff)
import Formatting ((%))
import Formatting qualified as F
import Oke.App.Bootstrap
import Oke.App.Context
import Oke.App.Logging
import Oke.Effect.Console
import Oke.Effect.Log

test :: IO ()
test = bracket initApp cleanupApp $
  \(Context bs _ cli, logger) -> runEff . runConsole . runLog logger $ do
    logDbg "Computing things..."
    logDbg "Sleeping..."
    logWarn "Computing more things..."
    printf (F.stext % " World\n") (show cli)
    logInfo (show bs)
    printf "enter anything: " *> flush
    msg <- getLn
    printf ("input: " % F.stext % "\n") msg

initApp :: IO (Context, Logger)
initApp = do
  ctx <- getContext
  logger <- mkLogger (mkLogPath ctx.bootstrap.xdgDirs.xdgState)
  pure $ (ctx, logger)

cleanupApp :: (Context, Logger) -> IO ()
cleanupApp (ctx, _) = cleanupLogger (mkLogPath ctx.bootstrap.xdgDirs.xdgState)
