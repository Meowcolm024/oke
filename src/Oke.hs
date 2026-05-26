module Oke (main) where

import Oke.App.CLI
import Oke.App.Env
import Oke.App.Run
import Oke.Core.Info qualified as Info
import Oke.Core.Registry qualified as Registry
import Oke.Effect
import Oke.Util.Misc (okeVersion)

main :: IO ()
main = runApp $ do
  ctx <- getCtx
  case ctx.cli of
    Info -> Info.info
    Version -> printLn okeVersion
    Update -> Registry.update
