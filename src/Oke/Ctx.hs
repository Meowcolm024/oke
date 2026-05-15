module Oke.Ctx (Ctx, getCtx, runCtx) where

import Context
import Effectful
import Effectful.Dispatch.Dynamic

data Ctx :: Effect where
  Ctx :: forall m. Ctx m Context

type instance DispatchOf Ctx = Dynamic

getCtx :: forall es. (Ctx :> es) => Eff es Context
getCtx = send Ctx

runCtx :: forall es a. (IOE :> es) => Context -> Eff (Ctx : es) a -> Eff es a
runCtx ctx = interpret $ \_ Ctx -> pure ctx
