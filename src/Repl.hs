module Repl
    ( perform
) where

import Control.Monad (unless)
import System.IO
import Config
import Env
import Commands
import Commands.Impl
import Commands.Lexer as Lexer
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
    manageCommand prog args
    where
        manageCommand prog args
            | prog == Lexer.replEmptyProg =
                purePerform
            | otherwise = do
                cmd <- Parsing.anyCommand prog args
                runCommand cmd
                {- Special handling for exit command, since it actually cannot have the control of the repl: it's not
                up to the exit command op-code to know the repl logic. -}
                unless (isExitCmd cmd) purePerform

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
