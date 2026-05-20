module Oke.Effect.Acid where

import Effectful

-- TODO acid operations
data Acid :: Effect

type instance DispatchOf Acid = Dynamic
