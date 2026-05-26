module Oke.Effect.FileSystem
  ( FileSystem,
    runFileSystem,
    doesFileExist,
    getModificationTime,
    withBinaryFile,
    hPutBS,
  )
where

import Data.Time (UTCTime)
import Effectful
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.FileSystem qualified as FS
import Effectful.FileSystem.IO qualified as FSIO
import Effectful.FileSystem.IO.ByteString qualified as FSIOBS
import Path

doesFileExist :: forall b es. (FileSystem :> es) => Path b File -> Eff es Bool
doesFileExist path = FS.doesFileExist (toFilePath path)

getModificationTime :: forall b es. (FileSystem :> es) => Path b File -> Eff es UTCTime
getModificationTime path = FS.getModificationTime (toFilePath path)

withBinaryFile :: forall a b es. (FileSystem :> es) => Path b File -> IOMode -> (Handle -> Eff es a) -> Eff es a
withBinaryFile path mode handle = FSIO.withBinaryFile (toFilePath path) mode (handle)

hPutBS :: forall es. (FileSystem :> es) => Handle -> ByteString -> Eff es ()
hPutBS handle bs = FSIOBS.hPut handle bs
