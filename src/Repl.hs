module Repl
    ( perform
) where

import Utils.Monad
import System.IO
import Config
import Env
import Commands.Impl
import Commands.Parsing as Parsing

emptyProg :: String
emptyProg = ""

fetchOpts :: String -> NotesKeeper (String, [String])
fetchOpts input =
    case words input of
        [] -> return (emptyProg, [])
        (w : ws) -> return (w, ws)

purePerform :: NotesKeeper ()
purePerform = retryOnError $ do
    displayPrompt
    cmdStr <- performIO getLine
    (prog, args) <- fetchOpts cmdStr
    if prog == emptyProg
    then doNothing
    else do
        cmd <- Parsing.anyCommand prog args
        runCommand cmd
    purePerform
    where
        displayPrompt = do
            display Config.replPrompt
            {- Flushing the prompt, since it has no newline character. -}
            performIO $ hFlush stdout

perform :: NotesKeeper ()
perform = do
    purePerform
    {- The changes are committed only at the end of the repl. This offers a better efficiency, since IO actions on the
    file-system could be expensive. -}
    commitChanges
