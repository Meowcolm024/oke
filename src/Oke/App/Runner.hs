{-# LANGUAGE TemplateHaskell #-}

module Oke.App.Runner where

import Effectful (runEff)
import Oke.App.CLI (getCli)
import Oke.App.Context
import Oke.Effect.Log
import Path (mkRelFile, (</>))

test :: IO ()
test = do
  hello <- getCli
  ctx <- newContext
  logger <- newLogger (ctx.dirs.xdgState </> $(mkRelFile "oke.log"))
  runEff . runLog logger $ do
    logDbg "Computing things..."
    logDbg "Sleeping..."
    logWarn "Computing more things..."
    logInfo $ hello <> " World"
    logInfo (show ctx)
