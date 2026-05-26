module Oke.Core.Query where

import Data.Aeson (decode')
import Data.Foldable (maximum)
import Data.FuzzySet
import Data.Text qualified as T
import Effectful
import Effectful.FileSystem.IO.ByteString.Lazy
import Oke.App.Env (registryPath)
import Oke.Core.Cask
import Oke.Effect
import Oke.Effect.FileSystem

data QueryResult = QueryResult
  { score :: !Double,
    caskInfo :: !CaskInfo
  }
  deriving stock (Show, Eq)

queryCasks :: forall m. (Monad m) => Text -> [CaskInfo] -> m [QueryResult]
queryCasks term registry = do
  res <- forM registry (runDefaultFuzzySearchT . fsearch)
  pure $ sortOn (Down . score) (concat res)
  where
    fsearch :: CaskInfo -> FuzzySearchT m [QueryResult]
    fsearch info = do
      mapM_ add_ (info.token : info.fullToken : info.name)
      res <- findOneMin 0.65 term
      case res of
        Nothing -> pure []
        Just (score, _) -> pure [QueryResult score info]

query :: forall es. (Log :> es, Console :> es, Ctx :> es, FileSystem :> es) => Text -> Eff es ()
query term = do
  logDbg $ "query: " <> term
  ctx <- getCtx
  exists <- doesFileExist (registryPath ctx)
  if not exists
    then
      logInfo "cask.json not found! Run `oke update` to fetch one."
    else do
      withBinaryFile (registryPath ctx) ReadMode $ \handle -> do
        reg <- hGetContents handle
        case decode' @[CaskInfo] reg of
          Nothing -> logErr "failed to parse cask.json"
          Just info -> do
            result <- queryCasks term info
            logDbg (show result)
            case result of
              [] -> printLn "No results found."
              rs ->
                let colWidth = maximum $ map (T.length . token . caskInfo) rs
                 in forM_ rs (printLn . formatRow colWidth)

formatRow :: Int -> QueryResult -> Text
formatRow colWidth (QueryResult {caskInfo = CaskInfo {token, name, version}}) =
  T.justifyLeft colWidth ' ' token <> " " <> case name of
    [] -> "<...>" <> version
    (n : _) -> n <> "@" <> version
