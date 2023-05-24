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

perform :: NotesKeeper ()
perform = retryOnError $ do
    displayPrompt
    cmdStr <- performIO getLine
    (prog, args) <- fetchOpts cmdStr
    if prog == emptyProg
    then doNothing
    else do
        cmd <- Parsing.anyCmd prog args
        runCommand cmd
    perform
    where
        displayPrompt = do
            display Config.replPrompt
            {- Flushing the prompt, since it has no newline character. -}
            performIO $ hFlush stdout
