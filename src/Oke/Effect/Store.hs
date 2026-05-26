module Oke.Effect.Store where

import Data.Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text.IO qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Path
import System.Directory
import System.FileLock
import System.Posix (rename)

data Store :: Type -> Effect where
  GetStore :: forall st m. Store st m st
  PutStore :: forall st m. st -> Store st m ()
  ModifyStore :: forall st m. (st -> st) -> Store st m ()

type instance DispatchOf (Store st) = Dynamic

runStore ::
  forall st a es.
  (IOE :> es, ToJSON st, FromJSON st) =>
  FileLock ->
  Path Abs File ->
  Eff (Store st : es) a ->
  Eff es a
runStore _ path = interpret $ \_ -> \case
  GetStore -> liftIO readState
  PutStore st -> liftIO $ writeAtomic st
  ModifyStore f -> liftIO $ readState >>= writeAtomic . f
  where
    statePath = fromAbsFile path

    readState :: IO st
    readState =
      eitherDecodeFileStrict statePath >>= \case
        Left err -> fail err -- TODO handle decode failure
        Right st -> pure st

    writeAtomic :: st -> IO ()
    writeAtomic st = do
      tmpPath <- fromAbsFile <$> addExtension ".tmp" path
      whenM (doesFileExist tmpPath) $ removeFile tmpPath
      BSL.writeFile tmpPath (encode st)
      rename tmpPath statePath

getStore :: forall st es. (Store st :> es) => Eff es st
getStore = send GetStore

putStore :: forall st es. (Store st :> es) => st -> Eff es ()
putStore st = send (PutStore st)

modifyStore :: forall st es. (Store st :> es) => (st -> st) -> Eff es ()
modifyStore f = send (ModifyStore f)

setupStore :: (ToJSON st) => st -> Path Abs File -> IO FileLock
setupStore st path = do
  let path' = fromAbsFile path
  exists <- doesFileExist path'
  unless exists $ BSL.writeFile path' (encode st)
  tryLockFile path' Exclusive >>= \case
    Nothing -> T.hPutStrLn stderr "Another instance is already running." *> exitFailure
    Just lk -> pure lk

unlockStore :: FileLock -> IO ()
unlockStore = unlockFile
