module Oke.Core.Error where

data RegError
  = RegistryNotFound
  | RegistryHashMismatch
  | RegistryParseFailure
  | StoreDecodeError !Text
  deriving stock (Show, Eq)

instance Exception RegError