module Oke.Bootstrap.CLI (getCLI, CLI (..)) where

import Options.Applicative

data CLI = Info
  deriving stock (Show, Eq)

getCLI :: IO CLI
getCLI = execParser $ info (options <**> helper) desc
  where
    desc = fullDesc <> header "oke - install homebrew casks without homebrew :D"

options :: Parser CLI
options = subparser (command "info" (info (pure Info) (progDesc "Print program info")))
