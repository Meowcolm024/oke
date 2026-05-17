module Oke.Effect.Context (Ctx (..), runContext, getContext, Context (..)) where

import Effectful
import Effectful.Dispatch.Dynamic
import Oke.Bootstrap.Context (Context (..))

data Ctx :: Effect where
  Ctx :: forall m. Ctx m Context

type instance DispatchOf Ctx = Dynamic

runContext :: Context -> Eff (Ctx : es) a -> Eff es a
runContext ctx = interpret $ \_ Ctx -> pure ctx

getContext :: forall es. (Ctx :> es) => Eff es Context
getContext = send Ctx
