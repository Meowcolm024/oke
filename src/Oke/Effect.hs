module Oke.Effect
  ( module Oke.Effect.Console,
    module Oke.Effect.Log,
    Ctx,
    runCtx,
    getCtx,
    Network,
    runNetwork,
    Time,
    runTime,
    FileSystem,
    runFileSystem,
  )
where

import Effectful
import Effectful.Reader.Static qualified as ER
import Oke.App.Env
import Oke.Effect.Console
import Oke.Effect.FileSystem (FileSystem, runFileSystem)
import Oke.Effect.Log
import Oke.Effect.Network (Network, runNetwork)
import Oke.Effect.Time (Time, runTime)

type Ctx = ER.Reader Context

runCtx :: forall a es. Context -> Eff (Ctx : es) a -> Eff es a
runCtx = ER.runReader

getCtx :: forall es. (Ctx :> es) => Eff es Context
getCtx = ER.ask
