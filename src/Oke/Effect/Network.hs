module Oke.Effect.Network (Network, runNetwork, request, download, downloadWithProgress) where

import Data.ByteString qualified as BS
import Data.List (lookup)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO (withBinaryFile)
import Effectful.FileSystem.IO.ByteString qualified as FS
import Effectful.FileSystem.IO.ByteString.Lazy qualified as LFS
import Formatting ((%))
import Formatting qualified as F
import Network.HTTP.Client qualified as C
import Network.HTTP.Types (hContentLength)
import Network.URI (URI)
import Oke.Effect.Log
import Path
import System.Console.ANSI

data Network :: Effect where
  Request :: forall m. URI -> Network m (C.Response LByteString)
  Download :: forall m. URI -> Path Abs File -> Network m ()
  DownloadWithProgress :: forall m. URI -> Path Abs File -> Network m ()

type instance DispatchOf Network = Dynamic

runNetwork :: forall es a. (IOE :> es, FileSystem :> es, Log :> es) => C.Manager -> Eff (Network : es) a -> Eff es a
runNetwork manager = interpret $ \_ -> \case
  (Request uri) -> do
    response <- liftIO $ do
      req <- C.requestFromURI uri
      C.httpLbs req manager
    pure response
  (Download uri path) -> do
    logDbg $ "Downloading from " <> (show uri)
    response <- liftIO $ do
      req <- C.requestFromURI uri
      C.httpLbs req manager
    LFS.writeFile (fromAbsFile path) (C.responseBody response)
  (DownloadWithProgress uri path) -> do
    logDbg $ "Downloading from " <> (show uri)
    rawReq <- liftIO $ C.requestFromURI uri
    let req = rawReq {C.decompress = const False}
    withBinaryFile (fromAbsFile path) WriteMode $ \handle -> do
      withRunInIO $ \run -> do
        C.withResponse req manager $ \response -> do
          let totalBytes = readMaybe @Int =<< (decodeUtf8 <$> lookup hContentLength (C.responseHeaders response))
          run $ logDbg $ "Downloading file with total size: " <> (maybe "???" show totalBytes)
          let bodyReader = C.responseBody response
          putText "Downloading..."
          hFlush stdout
          let loop total downloaded = do
                chunk <- C.brRead bodyReader
                unless (BS.null chunk) $ do
                  run $ FS.hPut handle chunk
                  let downloaded' = downloaded + fromIntegral (BS.length chunk)
                  progress total downloaded'
                  loop total downloaded'
          loop totalBytes 0
          putText "\n"
          hFlush stdout

request :: forall es. (Network :> es) => URI -> Eff es (C.Response LByteString)
request uri = send (Request uri)

download :: forall es. (Network :> es) => URI -> Path Abs File -> Eff es ()
download uri path = send (Download uri path)

downloadWithProgress :: forall es. (Network :> es) => URI -> Path Abs File -> Eff es ()
downloadWithProgress uri path = send (DownloadWithProgress uri path)

-- TODO parallel download

progress :: Maybe Int -> Int -> IO ()
progress Nothing _ = pure ()
progress (Just total) downloaded = do
  clearLine
  setCursorColumn 0
  let percent :: Double = (fromIntegral downloaded / fromIntegral total) * 100
  termSize <- getTerminalSize
  case termSize of
    Nothing -> putText $ F.sformat ("Downloading: " % F.fixed 2 % "%") percent
    Just (_, width) -> do
      let barWidth = if width < 40 then 0 else min 50 (width - 30)
          filled = round (percent / 100 * fromIntegral barWidth)
          progressBar = replicate filled '=' ++ replicate (barWidth - filled) ' '
      if barWidth > 0
        then putText $ F.sformat ("Downloading: [" % F.string % "] " % F.float % "%") progressBar percent
        else putText $ F.sformat ("Downloading: " % F.fixed 2 % "%") percent
  hFlush stdout
