module Commands.Impl
    ( seeCmd
    , takeCmd
    , runCommand
) where

import Utils.Monad (doNothing)
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
    displayLn ("---- NOTES ON " ++ path ++ " ----")
    displayLn ""
    displayLn' notes

takeCmd :: FilePath -> Text -> NotesKeeper ()
takeCmd path notes = do
    path' <- canonicalizePath' path
    checkFileExists path'
    displayLn ("---- taking notes on " ++ path' ++ " ----")
    overwriteEntry path' notes
    displayLn "done!"
    commitIfEagerMode

editCmd :: ProgName -> FilePath -> NotesKeeper ()
editCmd prog path = do
    path' <- canonicalizePath' path
    checkFileExists path'
    editEntryWith prog path'
    commitIfEagerMode

exitCmd :: NotesKeeper ()
exitCmd = doNothing

commitCmd :: NotesKeeper ()
commitCmd = commitChanges

loadCmd :: NotesKeeper ()
loadCmd = populateNotes

runCommand :: Command -> NotesKeeper ()
runCommand cmd =
    case cmd of
        SEE path -> seeCmd path
        TAKE path notes -> takeCmd path notes
        EDIT prog path -> editCmd prog path
        EXIT -> exitCmd
        COMMIT -> commitCmd
        LOAD -> loadCmd
        Seq cmd1 cmd2 -> do
            runCommand cmd1
            runCommand cmd2
