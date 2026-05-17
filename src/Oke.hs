module Oke (main) where

import Oke.Bootstrap.App
import Oke.Bootstrap.CLI
import Oke.Core.Info
import Oke.Effect.Context

main :: IO ()
main = runApp $ do
  ctx <- getContext
  case ctx.cli of
    Info -> info
