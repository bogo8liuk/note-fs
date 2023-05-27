module Commands
    ( Command(..)
    , isExitCmd
) where

import Utils.Fancy (ProgName)
import Data.Text

data Command =
      SEE FilePath
    | TAKE FilePath Text
    | EDIT ProgName FilePath
    | EXIT
    | COMMIT
    | LOAD
    {- Sequencing two commands -}
    | Seq Command Command

isExitCmd :: Command -> Bool
isExitCmd EXIT = True
isExitCmd _ = False
