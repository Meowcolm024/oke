module Oke.Core.Info where

import Data.Versions (prettyV)
import Effectful
import Formatting ((%))
import Formatting qualified as F
import Oke.App.Env
import Oke.Effect
import Oke.Util.Misc (okeVersion)
import Path (fromAbsFile)

info :: forall es. (Log :> es, Console :> es, Ctx :> es) => Eff es ()
info = do
  ctx <- getCtx
  logDbg "show info"
  printfn ("Version: " % F.stext) okeVersion
  printfn
    ("System info: " % F.stext % " (" % F.stext % ") " % F.stext)
    ( case ctx.system.platform.os of
        Darwin -> "macOS"
        Linux -> "linux"
    )
    (show ctx.system.platform.arch)
    (maybe "<unknown>" prettyV ctx.system.platform.ver)
  printfn ("Log location: " % F.string) (fromAbsFile . logPath $ ctx)
  printfn ("Config location: " % F.string) (fromAbsFile . configPath $ ctx)
  printfn ("Registry location: " % F.string) (fromAbsFile . registryPath $ ctx)
