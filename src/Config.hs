module Config
    ( relativeRootDirectory
    , relativeHistoryFile
    , relativeNotesFile
    , relativeEditingFile
    , notesFile
    , historyFile
    , editingFile
    , replPrompt
) where

import System.FilePath ((</>))

relativeRootDirectory :: FilePath
relativeRootDirectory = ".note-fs-work"

relativeHistoryFile :: FilePath
relativeHistoryFile = "history"

relativeNotesFile :: FilePath
relativeNotesFile = "notes"

relativeEditingFile :: FilePath
relativeEditingFile = "editing"

historyFile :: FilePath
historyFile = relativeRootDirectory </> relativeHistoryFile

notesFile :: FilePath
notesFile = relativeRootDirectory </> relativeNotesFile

editingFile :: FilePath
editingFile = relativeRootDirectory </> relativeEditingFile

replPrompt :: String
replPrompt = "> "
