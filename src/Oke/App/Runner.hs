module Oke.App.Runner where

import Control.Exception (bracket)
import Effectful (runEff)
import Formatting ((%))
import Formatting qualified as F
import Oke.App.Bootstrap
import Oke.App.CLI (getCli)
import Oke.App.Logging
import Oke.Effect.Console
import Oke.Effect.Log

test :: IO ()
test = bracket initApp cleanupApp $
  \(hello, ctx, logger) -> runEff . runConsole . runLog logger $ do
    logDbg "Computing things..."
    logDbg "Sleeping..."
    logWarn "Computing more things..."
    printf (F.stext % " World\n") hello
    logInfo (show ctx)
    printf "enter anything: " *> flush
    msg <- getLn
    printf ("input: " % F.stext % "\n") msg

initApp :: IO (Text, Bootstrap, Logger)
initApp = do
  ctx <- bootstrap
  hello <- getCli
  logger <- newLogger (mkLogPath ctx.xdgDirs.xdgState)
  pure $ (hello, ctx, logger)

cleanupApp :: (Text, Bootstrap, Logger) -> IO ()
cleanupApp (_, ctx, _) = cleanupLogger (mkLogPath ctx.xdgDirs.xdgState)
