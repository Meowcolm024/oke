module Oke.Effect.Time where

import Data.Time
import Data.Time.Clock qualified as T
import Effectful
import Effectful.Dispatch.Dynamic

data Time :: Effect where
  GetCurrentTime :: Time m UTCTime

type instance DispatchOf Time = Dynamic

runTime :: forall es a. (IOE :> es) => Eff (Time : es) a -> Eff es a
runTime = interpret $ \_ GetCurrentTime -> liftIO T.getCurrentTime

getCurrentTime :: forall es. (Time :> es) => Eff es UTCTime
getCurrentTime = send GetCurrentTime
