module Oke.Effect.Store where

import Data.Aeson
import Data.ByteString.Lazy qualified as BSL
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

runStore :: forall st a es. (IOE :> es, ToJSON st, FromJSON st) => st -> Path Abs File -> Eff (Store st : es) a -> Eff es a
runStore emptySt path = interpret $ \_ -> \case
  GetStore -> liftIO readState
  PutStore st -> liftIO $ withLock $ writeAtomic st
  ModifyStore f -> liftIO $ withLock $ readState >>= writeAtomic . f
  where
    statePath = fromAbsFile path

    readState :: IO st
    readState = do
      exists <- doesFileExist statePath
      if exists
        then
          eitherDecodeFileStrict statePath >>= \case
            Left err -> fail err -- TODO handle decode failure
            Right st -> pure st
        else pure emptySt

    writeAtomic :: st -> IO ()
    writeAtomic st = do
      tmpPath <- fromAbsFile <$> addExtension ".tmp" path
      whenM (doesFileExist tmpPath) $ removeFile tmpPath
      BSL.writeFile tmpPath (encode st)
      rename tmpPath statePath

    withLock :: forall e. IO e -> IO e
    withLock action = do
      lockPath <- fromAbsFile <$> addExtension ".lock" path
      withFileLock lockPath Exclusive (const action)

setupStore :: forall st. (ToJSON st) => st -> Path Abs File -> IO ()
setupStore st path = do
  let path' = fromAbsFile path
  exists <- doesFileExist path'
  unless exists $ BSL.writeFile path' (encode st)

getStore :: forall st es. (Store st :> es) => Eff es st
getStore = send GetStore

putStore :: forall st es. (Store st :> es) => st -> Eff es ()
putStore st = send (PutStore st)

modifyStore :: forall st es. (Store st :> es) => (st -> st) -> Eff es ()
modifyStore f = send (ModifyStore f)
