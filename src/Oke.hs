module Oke (main) where

import Formatting ((%))
import Formatting qualified as F
import Oke.App.CLI
import Oke.App.Env
import Oke.App.Run
import Oke.Core.Query qualified as Query
import Oke.Core.Registry qualified as Registry
import Oke.Core.Status qualified as Status
import Oke.Effect
import Oke.Effect.Error (handleRegErr)

main :: IO ()
main = runApp $ do
  ctx <- getCtx
  case ctx.cli of
    Version -> Status.version
    Status -> Status.status
    Update -> Registry.update
    Install token dry -> printfn ("TODO: install " % F.stext % " (" % F.stext % ")") token (show dry)
    Uninstall token dry -> printfn ("TODO: uninstall " % F.stext % " (" % F.stext % ")") token (show dry)
    Upgrade token dry -> printfn ("TODO: upgrade " % F.stext % " (" % F.stext % ")") token (show dry)
    Sync dry -> printfn ("TODO: sync (" % F.stext % ")") (show dry)
    Info token -> handleRegErr $ Query.info token
    Query token -> handleRegErr $ Query.query token
    ShellEnv -> printfn "TODO: shellenv"
