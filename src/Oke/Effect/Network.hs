module Oke.Effect.Network (Network, runNetwork, request, downloadSimple, downloadWithStatus) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Formatting ((%))
import Formatting qualified as F
import Network.HTTP.Client qualified as C
import Network.URI (URI)
import Oke.Effect.FileSystem
import Oke.Effect.Log
import Path
import System.Console.ANSI

data Network :: Effect where
  Request :: URI -> Network m (C.Response LByteString)
  Download :: URI -> Path Abs File -> Maybe (Integer -> IO ()) -> Network m ()

type instance DispatchOf Network = Dynamic

runNetwork ::
  forall es a.
  (IOE :> es, FileSystem :> es, Log :> es) =>
  C.Manager -> Eff (Network : es) a -> Eff es a
runNetwork manager = interpret $ \_ -> \case
  (Request uri) -> do
    response <- liftIO $ do
      req <- C.requestFromURI uri
      C.httpLbs req manager
    pure response
  (Download uri path renderer) -> do
    logDbg $ "Downloading from " <> show uri
    req <- liftIO $ C.requestFromURI uri
    withBinaryFile path WriteMode $ \handle ->
      withRunInIO $ \run ->
        C.withResponse req manager $ \response -> do
          let bodyReader = C.responseBody response
          putText $ F.sformat ("Fetching " % F.string % " ...") (fromRelFile (filename path))
          hFlush stdout
          let loop downloaded = do
                chunk <- C.brRead bodyReader
                unless (BS.null chunk) $ do
                  run $ hPutBS handle chunk
                  let downloaded' = downloaded + fromIntegral (BS.length chunk)
                  case renderer of
                    Just render -> render downloaded'
                    Nothing -> pure ()
                  loop downloaded'
          loop 0
          putText "\n"
          hFlush stdout

request :: forall es. (Network :> es) => URI -> Eff es (C.Response LByteString)
request uri = send (Request uri)

downloadSimple :: forall es. (Network :> es) => URI -> Path Abs File -> Eff es ()
downloadSimple uri path = send (Download uri path Nothing)

downloadWithStatus :: forall es. (Network :> es) => URI -> Path Abs File -> Eff es ()
downloadWithStatus uri path = send (Download uri path (Just render))
  where
    render bytes = do
      clearLine
      setCursorColumn 0
      termWidth <- maybe 80 snd <$> getTerminalSize
      let mbs = fromIntegral @Integer @Double bytes / 1024 / 1024
      let file = fromRelFile (filename path)
      let prefix = F.sformat ("Fetching " % F.string % " (" % F.fixed 2 % " MB) from ") file mbs
      let uri' = T.pack $ truncateMiddle (max 0 (termWidth - T.length prefix)) (show uri)
      putText $ prefix <> uri'
      hFlush stdout

    truncateMiddle maxWidth str
      | maxWidth <= 0 = ""
      | length str <= maxWidth = str
      | maxWidth <= 3 = take maxWidth str
      | otherwise =
          let keep = (maxWidth - 3) `div` 2
           in take keep str <> "..." <> drop (length str - keep) str
