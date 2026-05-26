module Oke.Core.Query where

import Data.Foldable (maximum)
import Data.FuzzySet
import Data.Text qualified as T
import Effectful
import Oke.Core.Cask
import Oke.Core.Registry (getRegistry)
import Oke.Core.State (AppState)
import Oke.Effect

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

query ::
  forall es.
  (Log :> es, Console :> es, Ctx :> es, FileSystem :> es, Store AppState :> es, Time :> es) =>
  Text -> Eff es ()
query term = do
  logDbg $ "query: " <> term
  reg <- getRegistry
  case reg of
    Nothing -> pure () -- do nothing
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
