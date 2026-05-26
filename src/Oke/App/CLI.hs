module Oke.App.CLI (CLI (..), DryRun (..), getCLI) where

import Options.Applicative

data DryRun = DryRun | LiveRun
  deriving stock (Show, Eq)

data CLI
  = Info
  | Update
  | Version
  | Install Text DryRun
  | Uninstall Text DryRun
  | Upgrade Text DryRun
  | Query Text
  | Sync DryRun
  | ShellEnv
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
        command "update" (info (pure Update) (progDesc "Update cask.json with Homebrew API")),
        command "install" (info (Install <$> caskArg <*> dryRun) (progDesc "Install a cask")),
        command "uninstall" (info (Uninstall <$> caskArg <*> dryRun) (progDesc "Uninstall a cask")),
        command "upgrade" (info (Upgrade <$> caskArg <*> dryRun) (progDesc "Upgrade a cask")),
        command "query" (info (Query <$> caskArg) (progDesc "Query info about a cask")),
        command "sync" (info (Sync <$> dryRun) (progDesc "Sync declared casks")),
        command "shell-env" (info (pure ShellEnv) (progDesc "Print shell environment exports"))
      ]

version :: Parser CLI
version = flag' Version (long "version" <> short 'v' <> help "Show program version")

dryRun :: Parser DryRun
dryRun = flag LiveRun DryRun (long "dry-run" <> short 'n' <> help "Print actions without executing")

caskArg :: Parser Text
caskArg = strArgument (metavar "CASK" <> help "Cask token (e.g. firefox)")
