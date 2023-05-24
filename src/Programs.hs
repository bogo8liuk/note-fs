module Programs
    ( repl
) where

import Data.Map.Strict
import System.Exit
import System.FilePath
import System.Directory (getHomeDirectory)
import Config
import Env
import Repl

setup :: FilePath -> FilePath -> IO NotesState
setup historyPath notesPath = do
    homeDir <- getHomeDirectory
    return $ NotesState
        { filesNotes = empty
        , isPopulated = False
        , notesPath = homeDir </> notesPath
        , historyPath = homeDir </> historyPath
        }

exitOnError :: Either err a -> IO a
exitOnError (Left _) = exitFailure
exitOnError (Right ok) = return ok

repl :: IO ()
repl = do
    st <- setup Config.historyFile Config.notesFile
    res <- runAsIO Repl.perform st
    {- This is done just for a better semantics. -}
    exitOnError res
    {- Not logging errors with the repl. -}
    return ()
