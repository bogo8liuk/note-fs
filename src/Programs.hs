module Programs
    ( repl
    , see
    , takeNote
) where

import Data.Map.Strict
import System.Exit (exitFailure)
import System.Environment (getProgName, getArgs)
import System.Directory (makeAbsolute)
import Config
import Env
import Repl
import Commands.Impl
import Commands.Parsing as Parsing

setup :: Mode -> FilePath -> FilePath -> IO NotesState
setup mode historyPath notesPath = do
    absHistoryPath <- makeAbsolute historyPath
    absNotesPath <- makeAbsolute notesPath
    return $ NotesState
        { filesNotes = empty
        , isPopulated = False
        , notesPath = absNotesPath
        , historyPath = absHistoryPath
        , mode = mode
        }

exitOnError :: Either err a -> IO a
exitOnError (Left _) = exitFailure
exitOnError (Right ok) = return ok

execCommand :: IO ()
execCommand = do
    st <- setup Exe Config.historyFile Config.notesFile
    prog <- getProgName
    args <- getArgs
    res <- runAsIO (parseCmdAndPerform prog args) st
    case res of
        Left err -> do
            print err
            exitFailure
        Right _ -> return ()
    where
        parseCmdAndPerform prog args = do
            cmd <- Parsing.anyCommand prog args
            runCommand cmd

{- The list of executables for single commands.
NB: the implementations of commands are all equal, but they internally differs because of the prog name. -}
see :: IO ()
see = execCommand

takeNote :: IO ()
takeNote = execCommand

repl :: IO ()
repl = do
    st <- setup Repl Config.historyFile Config.notesFile
    res <- runAsIO Repl.perform st
    {- This is done just for a better semantics. -}
    exitOnError res
    {- Not logging errors with the repl. -}
    return ()
