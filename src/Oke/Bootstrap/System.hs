module Oke.Bootstrap.System
  ( System (..),
    getSystem,
    XdgDirs (..),
    OSType (..),
    ArchType (..),
    Platform (..),
  )
where

import Control.Exception (catch)
import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import Data.Text.IO qualified as T
import Data.Versions qualified as V
import Oke.Util.Plist qualified as P
import Path
import System.Directory
import System.Info qualified as Info
import System.Posix.Unistd (SystemID (release), getSystemID)
import System.Posix.User (getEffectiveUserID, getEffectiveUserName)
import Text.Show qualified as TS

data System = System
  { xdgDirs :: !XdgDirs,
    platform :: !Platform,
    user :: Text
  }
  deriving stock (Show, Eq)

getSystem :: IO System
getSystem = do
  abortAtRoot
  dirs <- getXdgDirs
  platform <- getPlatform
  user <- getEffectiveUserName
  pure $ System dirs platform (T.pack user)

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
    ver :: !(Maybe V.Versioning)
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
    str -> fatal $ "Unsupported arch: " <> T.pack str
  ver <- catch (parseVer <$> getOSVer os) $ \(_ :: SomeException) -> do
    hPutStrLn stderr "Failed to retrive OS version."
    pure Nothing
  pure $ Platform os arch ver

parseVer :: Text -> Maybe V.Versioning
parseVer ver = case V.versioning (T.strip ver) of
  Right v -> pure v
  Left _ -> Nothing -- ignore parse error

getOSVer :: OSType -> IO Text
getOSVer os = case os of
  Linux -> T.pack . release <$> getSystemID
  Darwin -> do
    xml <- T.readFile "/System/Library/CoreServices/SystemVersion.plist"
    P.withPlistXML xml $ \node -> do
      verNode <- P.getDictItem node "ProductVersion"
      P.getString verNode

fatal :: Text -> IO a
fatal msg = hPutStrLn stderr ("[Fatal] " <> msg) *> exitFailure
