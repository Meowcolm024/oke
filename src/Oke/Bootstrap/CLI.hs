module Oke.Bootstrap.CLI (CLI (..), getCLI) where

import Options.Applicative

data CLI
  = Info
  | Update
  | Version
  deriving stock (Show, Eq)

getCLI :: IO CLI
getCLI = execParser $ info (options <**> helper) desc
  where
    desc = fullDesc <> header "oke - install homebrew casks without homebrew :D"

options :: Parser CLI
options = commands <|> version

commands :: Parser CLI
commands =
  subparser $
    mconcat
      [ command "info" (info (pure Info) (progDesc "Print program info")),
        command "update" (info (pure Update) (progDesc "Update cask.json with homebrew api"))
      ]

version :: Parser CLI
version = flag' Version (long "version" <> short 'v' <> help "Show program version")
