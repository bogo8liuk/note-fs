module Commands.Impl
    ( runCommand
    , seeCmd
    , takeNoteCmd
) where

import Data.Text
import Env
import Commands

runCommand :: Command -> NotesKeeper ()
runCommand cmd =
    case cmd of
        SEE path -> seeCmd path
        TAKE_NOTE path notes -> takeNoteCmd path notes

seeCmd :: FilePath -> NotesKeeper ()
seeCmd path = do
    notes <- getNotesOf path
    displayLn ""
    displayLn ("    ---- NOTES ON " ++ path ++ " ----")
    displayLn ""
    displayLn' notes

takeNoteCmd :: FilePath -> Text -> NotesKeeper ()
takeNoteCmd path notes = do
    checkFileExists path
    displayLn ("...taking notes on " ++ path ++ "...")
    appendNotes path notes
    overwriteEntry path notes
