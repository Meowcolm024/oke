module Oke.App.Runner where

import Control.Exception (bracket)
import Effectful (runEff)
import Oke.App.CLI (getCli)
import Oke.App.Context
import Oke.App.Logging
import Oke.Effect.Log
import System.Log.Logger (Logger)

test :: IO ()
test = bracket initApp cleanupApp $
  \(hello, ctx, logger) -> runEff . runLog logger $ do
    logDbg "Computing things..."
    logDbg "Sleeping..."
    logWarn "Computing more things..."
    logInfo $ hello <> " World"
    logInfo (show ctx)

initApp :: IO (Text, Context, Logger)
initApp = do
  hello <- getCli
  ctx <- newContext
  logger <- newLogger (mkLogPath ctx.dirs.xdgState)
  pure $ (hello, ctx, logger)

cleanupApp :: (Text, Context, Logger) -> IO ()
cleanupApp (_, ctx, _) = cleanupLogger (mkLogPath ctx.dirs.xdgState)
