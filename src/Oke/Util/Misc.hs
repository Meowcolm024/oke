module Oke.Util.Misc where

import Data.Text qualified as T
import Data.Version (showVersion)
import Paths_oke (version)

okeVersion :: Text
okeVersion = T.pack $ showVersion version
