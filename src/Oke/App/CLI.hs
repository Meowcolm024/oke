module Oke.App.CLI (getCli) where

import Options.Applicative

-- TODO cli options and parser

getCli :: IO Text
getCli = execParser $ info (pure "Hello" <**> helper) desc
  where
    desc = fullDesc <> header "oke - install homebrew casks without homebrew :D"