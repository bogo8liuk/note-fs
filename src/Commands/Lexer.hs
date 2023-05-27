module Commands.Lexer
    ( replExit
    , replSee
    , replTake
    , replEdit
    , replCommit
    , replLoad
    , exeSee
    , exeTake
    , exeEdit
) where

replExit :: String
replExit = "exit"

replSee :: String
replSee = "see"

replTake :: String
replTake = "take"

replEdit :: String
replEdit = "edit"

replCommit :: String
replCommit = "commit"

replLoad :: String
replLoad = "load"

exeSee :: String
exeSee = "nfs-see"

exeTake :: String
exeTake = "nfs-take"

exeEdit :: String
exeEdit = "nfs-edit"
