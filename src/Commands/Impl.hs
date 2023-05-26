module Commands.Impl
    ( seeCmd
    , takeCmd
    , runCommand
) where

import Utils.Fancy (ProgName)
import Data.Text
import Env
import Commands

{- In write-mode commands (so, for example, `see` is excluded), the changes are committed to the file-system only when
the mode is `Exe`. -}

seeCmd :: FilePath -> NotesKeeper ()
seeCmd path = do
    path' <- canonicalizePath' path
    notes <- getNotesOf path'
    displayLn ""
    displayLn ("    ---- NOTES ON " ++ path ++ " ----")
    displayLn ""
    displayLn' notes

takeCmd :: FilePath -> Text -> NotesKeeper ()
takeCmd path notes = do
    path' <- canonicalizePath' path
    checkFileExists path'
    displayLn ("...taking notes on " ++ path' ++ "...")
    overwriteEntry path' notes
    commitIfExeMode

editCmd :: ProgName -> FilePath -> NotesKeeper ()
editCmd prog path = do
    path' <- canonicalizePath' path
    checkFileExists path'
    editEntryWith prog path'
    commitIfExeMode

runCommand :: Command -> NotesKeeper ()
runCommand cmd =
    case cmd of
        SEE path -> seeCmd path
        TAKE path notes -> takeCmd path notes
        EDIT prog path -> editCmd prog path
