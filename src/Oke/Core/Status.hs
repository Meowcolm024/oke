module Oke.Core.Status where

import Data.Version (showVersion)
import Data.Versions (prettyV)
import Effectful
import Formatting ((%))
import Formatting qualified as F
import Oke.App.Env
import Oke.Effect
import Oke.Effect.FileSystem (doesFileExist)
import Path
import Paths_oke qualified

version :: forall es. (Console :> es) => Eff es ()
version = printfn F.string (showVersion Paths_oke.version)

status :: forall es. (Log :> es, Console :> es, Ctx :> es, FileSystem :> es) => Eff es ()
status = do
  ctx <- getCtx
  logDbg "show info"
  printfn ("Version: " % F.string) (showVersion Paths_oke.version)
  printfn
    ("System info: " % F.stext % " (" % F.stext % ") " % F.stext)
    ( case ctx.system.platform.os of
        Darwin -> "macOS"
        Linux -> "linux"
    )
    (show ctx.system.platform.arch)
    (maybe "<unknown>" prettyV ctx.system.platform.ver)
  printfn ("Log location: " % F.string) =<< showExists (logPath ctx)
  printfn ("Config location: " % F.string) =<< showExists (configPath ctx)
  printfn ("Registry location: " % F.string) =<< showExists (registryPath ctx)

showExists :: forall es. (FileSystem :> es) => Path Abs File -> Eff es String
showExists path = do
  exists <- doesFileExist path
  pure $ if exists then fromAbsFile path else ""
