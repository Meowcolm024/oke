{-# LANGUAGE TemplateHaskell #-}

module Context
  ( Context (..),
    newContext,
    Dirs (..),
    SupportedOS (..),
    SupportedArch (..),
    Platform (..),
  )
where

import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import Path
import System.Directory
import System.Info qualified as Info
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (LogHandler (setFormatter))
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger qualified as L
import System.Posix.User (getEffectiveUserID)
import System.Process (readProcess)

data Context = Context
  { dirs :: Dirs,
    platform :: Platform,
    logger :: L.Logger
  }

newContext :: IO Context
newContext = do
  abortAtRoot
  dirs <- newDirs
  platform <- newPlatform
  logger <- newLogger (xdgState dirs </> $(mkRelFile "oke.log"))
  pure $ Context dirs platform logger

abortAtRoot :: IO ()
abortAtRoot = do
  euid <- getEffectiveUserID
  when (euid == 0) $
    fatal "This program should not be run as root!"

data Dirs = Dirs
  { xdgState :: Path Abs Dir,
    xdgConfig :: Path Abs Dir,
    xdgCache :: Path Abs Dir
  }
  deriving stock (Show, Eq)

newDirs :: IO Dirs
newDirs = do
  xdgState <- parseAbsDir =<< getXdgDirectory XdgState "oke"
  xdgConfig <- parseAbsDir =<< getXdgDirectory XdgConfig "oke"
  xdgCache <- parseAbsDir =<< getXdgDirectory XdgCache "oke"
  createDirectoryIfMissing False (fromAbsDir xdgState)
  createDirectoryIfMissing False (fromAbsDir xdgConfig)
  createDirectoryIfMissing False (fromAbsDir xdgCache)
  pure $ Dirs xdgState xdgConfig xdgCache

data SupportedOS = Darwin | Linux deriving stock (Show, Eq)

data SupportedArch = ARM | X86 deriving stock (Show, Eq)

data Platform = Platform
  { os :: SupportedOS,
    arch :: SupportedArch,
    majorVer :: Int
  }
  deriving stock (Show, Eq)

newPlatform :: IO Platform
newPlatform = do
  os <- case Info.os of
    "darwin" -> pure Darwin
    "linux" -> pure Linux
    _ -> fatal "unsupported os"
  arch <- case Info.arch of
    "aarch64" -> pure ARM
    "x86_64" -> pure X86
    _ -> fatal "unsupported arch"
  majorVer <- case os of
    Darwin -> readProcess "sw_vers" ["-productVersion"] "" >>= getMajorVer
    Linux -> readProcess "uname" ["-r"] "" >>= getMajorVer
  pure $ Platform os arch majorVer

getMajorVer :: String -> IO Int
getMajorVer ver = do
  case span (/= '.') ver of
    (major, _) -> case readMaybe major of
      Just v -> pure v
      Nothing -> fatal $ "Failed to parse major version: " <> T.pack ver

newLogger :: Path Abs File -> IO L.Logger
newLogger path = do
  chdl <- streamHandler stdout L.INFO
  let chFmt = setFormatter chdl (simpleLogFormatter "[$prio] $msg")
  fhdl <- fileHandler (fromAbsFile path) L.DEBUG
  let fhFmt = setFormatter fhdl (simpleLogFormatter "$time [$prio] $msg")
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG . L.setHandlers [chFmt, fhFmt])
  L.getRootLogger

fatal :: T.Text -> IO a
fatal msg = hPutStrLn stderr ("[Fatal] " <> msg) >> exitFailure