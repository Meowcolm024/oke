module Oke.Effect.Console
  ( Console,
    runConsole,
    printf,
    printfn,
    printLn,
    getLn,
    flush,
  )
where

import Data.Text.Lazy.Builder qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import Formatting qualified as F

data Console :: Effect where
  PrintConsole :: forall m. Text -> Console m ()
  ReadConsole :: forall m. Console m Text
  FlushConsole :: forall m. Console m ()

-- TODO ANSI

type instance DispatchOf Console = Dynamic

runConsole :: forall es a. (IOE :> es) => Eff (Console : es) a -> Eff es a
runConsole = interpret $ \_ -> \case
  PrintConsole msg -> liftIO $ putText msg
  ReadConsole -> liftIO $ getLine
  FlushConsole -> liftIO $ hFlush stdout

printf :: forall es a. (Console :> es) => F.Format (Eff es ()) a -> a
printf fmt = F.runFormat fmt (\b -> send (PrintConsole $ fromLazy (T.toLazyText b)))

printfn :: forall es a. (Console :> es) => F.Format (Eff es ()) a -> a
printfn fmt = F.runFormat fmt (\b -> send (PrintConsole $ fromLazy (T.toLazyText b <> "\n")))

printLn :: forall es. (Console :> es) => Text -> Eff es ()
printLn msg = send (PrintConsole (msg <> "\n"))

getLn :: forall es. (Console :> es) => Eff es Text
getLn = send ReadConsole

flush :: forall es. (Console :> es) => Eff es ()
flush = send FlushConsole
