module Config
    ( relativeRootDirectory
    , relativeHistoryFile
    , relativeNotesFile
    , notesFile
    , historyFile
    , replPrompt
) where

import System.FilePath ((</>))

relativeRootDirectory :: FilePath
relativeRootDirectory = ".note-fs-work"

relativeHistoryFile :: FilePath
relativeHistoryFile = "history"

relativeNotesFile :: FilePath
relativeNotesFile = "notes"

historyFile :: FilePath
historyFile = relativeRootDirectory </> relativeHistoryFile

notesFile :: FilePath
notesFile = relativeRootDirectory </> relativeNotesFile

replPrompt :: String
replPrompt = "> "
