module Oke where

import Context
import Effectful (runEff)
import Oke.Ctx
import Oke.Log

test :: IO ()
test = do
  rawCtx <- newContext
  runEff . runCtx rawCtx . runLog rawCtx.logger $ do
    logDbg "Computing things..."
    logDbg "Sleeping..."
    logWarn "Computing more things..."
    logInfo "Hello World"
    ctx <- getCtx
    logInfo (show ctx.dirs)
    logInfo (show ctx.platform)
