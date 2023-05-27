module Repl
    ( perform
) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import System.Console.Haskeline hiding (display)
import Config hiding (historyFile)
import Env
import Commands
import Commands.Impl
import Commands.Lexer as Lexer
import Commands.Parsing as Parsing

emptyProg :: String
emptyProg = ""

fetchOpts :: String -> (String, [String])
fetchOpts input =
    {- TODO: using `words` is wrong, because arguments can be enclosed between "" -}
    case words input of
        [] -> (emptyProg, [])
        (w : ws) -> (w, ws)

replInput :: InputT NotesKeeper ()
replInput = do
    input <- getInputLine Config.replPrompt
    case input of
        {- `Nothing` has the semantics of "end-of-file", which is, then, implemented as the exit command -}
        Nothing -> runCommand' EXIT
        Just cmdStr -> do
            let (prog, args) = fetchOpts cmdStr
            isEnd <- continueOnError $ manageCommand prog args
            unless isEnd replInput
    where
        continueOnError op =
            {- Returning `False` in case of recoverable errors, because the semantics is not the end of the repl -}
            lift (op `catchError` handleErrorWith (return False))

        manageCommand prog args
            | prog == Lexer.replEmptyProg =
                return False
            | otherwise = do
                cmd <- Parsing.anyCommand prog args
                runCommand cmd
                {- Special handling for exit command, since it actually cannot have the control of the repl: it's not
                up to the exit command op-code to know the repl logic. -}
                return $ isExitCmd cmd

        runCommand' = lift . runCommand

{-
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
-}

settings :: FilePath -> Settings NotesKeeper
settings history =
    defaultSettings { historyFile = Just history }

perform :: NotesKeeper ()
perform = do
    history <- getHistoryPath
    runInputT (settings history) replInput
    {- The changes are committed only at the end of the repl. This offers a better efficiency, since IO actions on the
    file-system could be expensive. -}
    commitChanges
