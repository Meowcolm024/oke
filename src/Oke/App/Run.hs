module Oke.App.Run where

import Control.Exception (bracket)
import Data.Text.IO (hPutStrLn)
import Effectful
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Oke.App.Env
import Oke.Core.State (AppState, emptyAppState)
import Oke.Effect
import Oke.Effect.Store
import System.Log.Logger (Logger)
import System.Posix.User (getEffectiveUserID)

type App = Eff '[Store AppState, Network, Log, Ctx, FileSystem, Time, Console, IOE] ()

runAppWith :: (Context, Logger, Manager) -> App -> IO ()
runAppWith (ctx, logger, manager) =
  runEff
    . runConsole
    . runTime
    . runFileSystem
    . runCtx ctx
    . runLog logger
    . runNetwork manager
    . runStore emptyAppState (statePath ctx)

runApp :: App -> IO ()
runApp app = abortAtRoot *> bracket initApp cleanupApp (`runAppWith` app)
  where
    initApp :: IO (Context, Logger, Manager)
    initApp = do
      ctx <- mkContext
      logger <- mkLogger (logPath ctx)
      manager <- newTlsManager
      setupStore emptyAppState (statePath ctx)
      pure $ (ctx, logger, manager)

    cleanupApp :: (Context, Logger, Manager) -> IO ()
    cleanupApp (ctx, _, _) = cleanupLogger (logPath ctx)

    abortAtRoot :: IO ()
    abortAtRoot = do
      euid <- getEffectiveUserID
      when (euid == 0) $ do
        hPutStrLn stderr "This program should not be run as root!"
        exitFailure
