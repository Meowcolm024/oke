module Oke (main) where

import Oke.Bootstrap.App
import Oke.Bootstrap.CLI
import Oke.Core.Info qualified as Info
import Oke.Core.Registry qualified as Registry
import Oke.Effect.Console (printLn)
import Oke.Effect.Context
import Oke.Util.Misc (okeVersion)

main :: IO ()
main = runApp $ do
  ctx <- getContext
  case ctx.cli of
    Info -> Info.info
    Version -> printLn okeVersion
    Update -> Registry.update
