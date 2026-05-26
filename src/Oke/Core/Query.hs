module Oke.Core.Query where

import Data.Foldable (maximum)
import Data.FuzzySet
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Formatting ((%))
import Formatting qualified as F
import Oke.Core.Cask
import Oke.Core.Registry
import Oke.Core.State (AppState)
import Oke.Effect

data QueryError = QueryError Text [Text]
  deriving stock (Show, Eq)

instance Exception QueryError

handleQueryError :: forall es. (Log :> es) => Eff (Error QueryError : es) () -> Eff es ()
handleQueryError = runErrorNoCallStackWith $ \(QueryError term rs) ->
  logErr $ F.sformat ("Cask '" % F.stext % "' not found, similar casks: " % F.stext) term (T.unwords rs)

data QueryResult = QueryResult
  { score :: !Double,
    caskInfo :: !CaskInfo
  }
  deriving stock (Show, Eq)

queryCasks :: forall m. (Monad m) => Text -> Registry -> m [QueryResult]
queryCasks term registry = do
  res <- forM registry (runDefaultFuzzySearchT . fsearch)
  pure $ sortOn (Down . score) (concat res)
  where
    fsearch :: CaskInfo -> FuzzySearchT m [QueryResult]
    fsearch caskInfo@(CaskInfo {token, fullToken, name}) = do
      mapM_ add_ (token : fullToken : name)
      res <- findOneMin 0.65 term
      case res of
        Nothing -> pure []
        Just (score, _) -> pure [QueryResult score caskInfo]

exactCask :: forall es. (Error QueryError :> es) => Text -> Registry -> Eff es CaskInfo
exactCask term registry = do
  result <- queryCasks term registry
  case result of
    (QueryResult {caskInfo} : _) | caskInfo.token == term -> pure caskInfo
    rs -> throwError $ QueryError term (map (token . caskInfo) rs)

query ::
  forall es.
  ( Log :> es,
    Console :> es,
    Ctx :> es,
    FileSystem :> es,
    Store AppState :> es,
    Time :> es,
    Error RegError :> es
  ) =>
  Text -> Eff es ()
query term = do
  logDbg $ "query: " <> term
  registry <- getRegistry
  result <- queryCasks term registry
  logDbg (show result)
  case result of
    [] -> printLn "No results found."
    rs ->
      let colWidth = maximum $ map (T.length . token . caskInfo) rs
       in forM_ rs (printLn . formatRow colWidth)

info ::
  forall es.
  ( Log :> es,
    Console :> es,
    Ctx :> es,
    FileSystem :> es,
    Store AppState :> es,
    Time :> es,
    Error RegError :> es,
    Error QueryError :> es
  ) =>
  Text -> Eff es ()
info term = do
  logDbg $ "info: " <> term
  registry <- getRegistry
  cask <- exactCask term registry
  logDbg (show cask)
  printfn ("Token: " % F.stext) cask.token
  printfn ("Name: " % F.stext) (T.unwords cask.name)
  printfn ("Version: " % F.stext) cask.version
  printfn ("Description: " % F.stext) cask.desc
  printfn ("Homepage: " % F.stext) cask.homepage

formatRow :: Int -> QueryResult -> Text
formatRow colWidth (QueryResult {caskInfo = CaskInfo {token, name, version}}) =
  T.justifyLeft colWidth ' ' token <> " " <> case name of
    [] -> "<...>" <> version
    (n : _) -> n <> " " <> version
