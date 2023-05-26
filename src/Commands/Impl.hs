module Commands.Impl
    ( seeCmd
    , takeCmd
    , runCommand
) where

import Data.Text
import Env
import Commands

seeCmd :: FilePath -> NotesKeeper ()
seeCmd path = do
    notes <- getNotesOf path
    displayLn ""
    displayLn ("    ---- NOTES ON " ++ path ++ " ----")
    displayLn ""
    displayLn' notes

takeCmd :: FilePath -> Text -> NotesKeeper ()
takeCmd path notes = do
    checkFileExists path
    displayLn ("...taking notes on " ++ path ++ "...")
    appendNotes path notes
    overwriteEntry path notes

editCmd :: ProgName -> FilePath -> NotesKeeper ()
editCmd prog path = do
    checkFileExists path
    undefined --TODO

runCommand :: Command -> NotesKeeper ()
runCommand cmd =
    case cmd of
        SEE path -> seeCmd path
        TAKE path notes -> takeCmd path notes
        EDIT prog path -> editCmd prog path
