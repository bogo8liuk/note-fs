module Commands
    ( Command(..)
) where

import Data.Text

data Command =
      SEE FilePath
    | TAKE_NOTE FilePath Text
