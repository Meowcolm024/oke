module Oke.Bootstrap.App where

import Control.Exception (bracket)
import Data.Text.IO (hPutStrLn)
import Effectful
import Effectful.FileSystem
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Oke.Bootstrap.Context
import Oke.Bootstrap.Log
import Oke.Effect.Console
import Oke.Effect.Context
import Oke.Effect.Log
import Oke.Effect.Network
import System.Posix.User (getEffectiveUserID)

type App = Eff '[Network, Log, Ctx, FileSystem, Console, IOE] ()

runAppWith :: (Context, Logger, Manager) -> App -> IO ()
runAppWith (ctx, logger, manager) = runEff . runConsole . runFileSystem . runContext ctx . runLog logger . runNetwork manager

runApp :: App -> IO ()
runApp app = abortAtRoot *> bracket initApp cleanupApp (`runAppWith` app)
  where
    initApp :: IO (Context, Logger, Manager)
    initApp = do
      ctx <- mkContext
      logger <- mkLogger (logPath ctx)
      manager <- newTlsManager
      pure $ (ctx, logger, manager)

    cleanupApp :: (Context, Logger, Manager) -> IO ()
    cleanupApp (ctx, _, _) = cleanupLogger (logPath ctx)

    abortAtRoot :: IO ()
    abortAtRoot = do
      euid <- getEffectiveUserID
      when (euid == 0) $ do
        hPutStrLn stderr "This program should not be run as root!"
        exitFailure
