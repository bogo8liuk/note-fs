module Programs
    ( repl
    , seeExec
    , takeExec
) where

import Data.Map.Strict
import System.Exit (exitFailure)
import System.Environment (getProgName, getArgs)
import System.FilePath ((</>))
import System.Directory (getHomeDirectory)
import Config
import Env
import Repl
import Commands.Impl
import Commands.Parsing as Parsing

setup :: Mode -> FilePath -> FilePath -> FilePath -> IO NotesState
setup mode historyPath notesPath editPath = do
    homeDir <- getHomeDirectory
    let absHistoryPath = homeDir </> historyPath
    let absNotesPath = homeDir </> notesPath
    let absEditPath = homeDir </> editPath
    return $ NotesState
        { filesNotes = empty
        , isPopulated = False
        , notesPath = absNotesPath
        , historyPath = absHistoryPath
        , editingPath = absEditPath
        , mode = mode
        }

exitOnError :: Either err a -> IO a
exitOnError (Left _) = exitFailure
exitOnError (Right ok) = return ok

execCommand :: IO ()
execCommand = do
    st <- setup Exe Config.historyFile Config.notesFile Config.editingFile
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
seeExec :: IO ()
seeExec = execCommand

takeExec :: IO ()
takeExec = execCommand

repl :: IO ()
repl = do
    st <- setup Repl Config.historyFile Config.notesFile Config.editingFile
    res <- runAsIO Repl.perform st
    {- This is done just for a better semantics. -}
    exitOnError res
    {- Not logging errors with the repl. -}
    return ()
