module Oke.Bootstrap.App where

import Control.Exception (bracket)
import Effectful
import Oke.Bootstrap.Context
import Oke.Bootstrap.Log
import Oke.Effect.Console
import Oke.Effect.Context
import Oke.Effect.Log

type App = Eff '[Log, Ctx, Console, IOE] ()

runAppWith :: (Context, Logger) -> App -> IO ()
runAppWith (ctx, logger) = runEff . runConsole . runContext ctx . runLog logger

runApp :: App -> IO ()
runApp app = bracket initApp cleanupApp (`runAppWith` app)
  where
    initApp :: IO (Context, Logger)
    initApp = do
      ctx <- mkContext
      logger <- mkLogger ctx.logPath
      pure $ (ctx, logger)

    cleanupApp :: (Context, Logger) -> IO ()
    cleanupApp (ctx, _) = cleanupLogger ctx.logPath
