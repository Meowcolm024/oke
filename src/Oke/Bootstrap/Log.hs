{-# LANGUAGE TemplateHaskell #-}

module Oke.Bootstrap.Log (Logger, LogPath, mkLogger, cleanupLogger) where

import Data.ByteString qualified as B
import Path
import System.Directory
import System.IO
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (LogHandler (setFormatter))
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger (Logger)
import System.Log.Logger qualified as L

type LogPath = Path Abs File

mkLogger :: LogPath -> IO Logger
mkLogger logPath = do
  chdl <- streamHandler stdout L.INFO
  let chFmt = setFormatter chdl (simpleLogFormatter "[$prio] $msg")
  fhdl <- fileHandler (fromAbsFile logPath) L.DEBUG
  let fhFmt = setFormatter fhdl (simpleLogFormatter "$time [$prio] $msg")
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG . L.setHandlers [chFmt, fhFmt])
  L.getRootLogger

cleanupLogger :: LogPath -> IO ()
cleanupLogger logPath = do
  L.removeAllHandlers
  trimLogFile logPath

trimLogFile :: LogPath -> IO ()
trimLogFile absPath = do
  let path = fromAbsFile absPath
  exists <- doesFileExist path
  when exists $ do
    size <- getFileSize path
    when (size > maxBytes) $ do
      withFile path ReadMode $ \h -> do
        hSeek h SeekFromEnd (-keepBytes)
        !recentLog <- B.hGetContents h
        let notice = "[... Log truncated for size ...]\n"
        B.writeFile path (notice <> recentLog)
  where
    maxBytes = 2 * 1024 * 1024
    keepBytes = 512 * 1024
