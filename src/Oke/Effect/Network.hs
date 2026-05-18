module Oke.Effect.Network where

import Data.ByteString qualified as BS
import Effectful
import Effectful.Dispatch.Dynamic
import Network.HTTP.Client qualified as C
import Oke.Effect.Console
import Oke.Effect.Log

data Network :: Effect where
  Request :: forall m. C.Request -> Network m ByteString

type instance DispatchOf Network = Dynamic

runNetwork :: forall es a. (IOE :> es, Log :> es, Console :> es) => C.Manager -> Eff (Network : es) a -> Eff es a
runNetwork manager = interpret $ \_ (Request req) -> do
  response <- liftIO $ C.httpLbs req manager
  pure $ BS.toStrict (C.responseBody response)

request :: forall es. (IOE :> es, Network :> es) => Text -> Eff es ByteString
request url = do
  req <- C.parseRequest (toString url)
  send (Request req)