module Oke.App.Bootstrap
  ( Bootstrap (..),
    bootstrap,
    XdgDirs (..),
    OSType (..),
    ArchType (..),
    Platform (..),
  )
where

import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import Path
import System.Directory
import System.Info qualified as Info
import System.Posix.User (getEffectiveUserID, getEffectiveUserName)
import System.Process (readProcess)
import Text.Show qualified as TS

data Bootstrap = Bootstrap
  { xdgDirs :: !XdgDirs,
    platform :: !Platform,
    user :: Text
  }
  deriving stock (Show, Eq)

bootstrap :: IO Bootstrap
bootstrap = do
  abortAtRoot
  dirs <- getXdgDirs
  platform <- getPlatform
  user <- getEffectiveUserName
  pure $ Bootstrap dirs platform (T.pack user)

abortAtRoot :: IO ()
abortAtRoot = do
  euid <- getEffectiveUserID
  when (euid == 0) $
    fatal "This program should not be run as root!"

data XdgDirs = XdgDirs
  { xdgState :: Path Abs Dir,
    xdgConfig :: Path Abs Dir,
    xdgCache :: Path Abs Dir
  }
  deriving stock (Show, Eq)

getXdgDirs :: IO XdgDirs
getXdgDirs = do
  xdgState <- parseAbsDir =<< getXdgDirectory XdgState "oke"
  xdgConfig <- parseAbsDir =<< getXdgDirectory XdgConfig "oke"
  xdgCache <- parseAbsDir =<< getXdgDirectory XdgCache "oke"
  createDirectoryIfMissing False (fromAbsDir xdgState)
  createDirectoryIfMissing False (fromAbsDir xdgConfig)
  createDirectoryIfMissing False (fromAbsDir xdgCache)
  pure $ XdgDirs xdgState xdgConfig xdgCache

data OSType = Darwin | Linux deriving stock (Eq)

instance TS.Show OSType where
  show Darwin = "darwin"
  show Linux = "linux"

data ArchType = ARM | X86 deriving stock (Eq)

instance TS.Show ArchType where
  show ARM = "aarch64"
  show X86 = "x86_64"

data Platform = Platform
  { os :: !OSType,
    arch :: !ArchType,
    majorVer :: !Int
  }
  deriving stock (Show, Eq)

getPlatform :: IO Platform
getPlatform = do
  os <- case Info.os of
    "darwin" -> pure Darwin
    "linux" -> pure Linux
    str -> fatal $ "Unsupported os: " <> T.pack str
  arch <- case Info.arch of
    "aarch64" -> pure ARM
    "x86_64" -> pure X86
    str -> fatal $ "unsupported arch: " <> T.pack str
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

fatal :: Text -> IO a
fatal msg = hPutStrLn stderr ("[Fatal] " <> msg) >> exitFailure
