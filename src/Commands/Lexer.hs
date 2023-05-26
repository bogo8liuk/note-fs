module Commands.Lexer
    ( replEmptyProg
    , replExit
    , replSee
    , replTake
    , replEdit
    , exeSee
    , exeTake
    , exeEdit
) where

replEmptyProg :: String
replEmptyProg = ""

replExit :: String
replExit = "exit"

replSee :: String
replSee = "see"

replTake :: String
replTake = "take"

replEdit :: String
replEdit = "edit"

exeSee :: String
exeSee = "nfs-see"

exeTake :: String
exeTake = "nfs-take"

exeEdit :: String
exeEdit = "nfs-edit"
