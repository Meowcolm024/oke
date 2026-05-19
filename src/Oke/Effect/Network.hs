module Oke.Effect.Network where

import Effectful
import Effectful.Dispatch.Dynamic
import Network.HTTP.Client qualified as C
import Network.URI (URI)
import Path

data Network :: Effect where
  Request :: forall m. URI -> Network m ByteString
  Download :: forall m. URI -> Path Abs File -> Network m ()

type instance DispatchOf Network = Dynamic

runNetwork :: forall es a. (IOE :> es) => C.Manager -> Eff (Network : es) a -> Eff es a
runNetwork manager = interpret $ \_ -> \case
  (Request uri) -> do
    response <- liftIO $ do
      req <- C.requestFromURI uri
      C.httpLbs req manager
    -- TODO maybe we can put some logs about request/response data
    pure $ toStrict (C.responseBody response)
  (Download _ _) -> pure ()

request :: forall es. (Network :> es) => URI -> Eff es ByteString
request uri = send (Request uri)

download :: forall es. (Network :> es) => URI -> Path Abs File -> Eff es ()
download uri path = send (Download uri path)

-- TODO download with progress bar
-- TODO (much later) parallel download
