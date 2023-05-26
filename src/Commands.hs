module Commands
    ( Command(..)
) where

import Utils.Fancy (ProgName)
import Data.Text

data Command =
      SEE FilePath
    | TAKE FilePath Text
    | EDIT ProgName FilePath
