module Config
    ( relativeRootDirectory
    , relativeHistoryFile
    , relativeNotesFile
    , relativeEditingFileBase
    , notesFile
    , historyFile
    , editingFileBase
    , replPrompt
) where

import System.FilePath ((</>))

relativeRootDirectory :: FilePath
relativeRootDirectory = ".note-fs-work"

relativeHistoryFile :: FilePath
relativeHistoryFile = "history"

relativeNotesFile :: FilePath
relativeNotesFile = "notes"

relativeEditingFileBase :: FilePath
relativeEditingFileBase = "editing"

historyFile :: FilePath
historyFile = relativeRootDirectory </> relativeHistoryFile

notesFile :: FilePath
notesFile = relativeRootDirectory </> relativeNotesFile

editingFileBase :: FilePath
editingFileBase = relativeRootDirectory </> relativeEditingFileBase

replPrompt :: String
replPrompt = "> "
