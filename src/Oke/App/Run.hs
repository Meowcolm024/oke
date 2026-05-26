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
import System.FileLock (FileLock)
import System.Log.Logger (Logger)
import System.Posix.User (getEffectiveUserID)

type App = Eff '[Store AppState, Network, Log, Ctx, FileSystem, Time, Console, IOE] ()

runAppWith :: (Context, Logger, Manager, FileLock) -> App -> IO ()
runAppWith (ctx, logger, manager, lock) =
  runEff
    . runConsole
    . runTime
    . runFileSystem
    . runCtx ctx
    . runLog logger
    . runNetwork manager
    . runStore lock (statePath ctx)

runApp :: App -> IO ()
runApp app = abortAtRoot *> bracket initApp cleanupApp (`runAppWith` app)
  where
    initApp :: IO (Context, Logger, Manager, FileLock)
    initApp = do
      ctx <- setupContext
      logger <- setupLogger (logPath ctx)
      manager <- newTlsManager
      lock <- setupStore emptyAppState (statePath ctx)
      pure (ctx, logger, manager, lock)

    cleanupApp :: (Context, Logger, Manager, FileLock) -> IO ()
    cleanupApp (ctx, _, _, lock) = do
      unlockStore lock
      cleanupLogger (logPath ctx)

    abortAtRoot :: IO ()
    abortAtRoot = do
      euid <- getEffectiveUserID
      when (euid == 0) $ do
        hPutStrLn stderr "This program should not be run as root!"
        exitFailure
