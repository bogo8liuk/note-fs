module Commands.Parsing
    ( seeCmd
    , takeCmd
    , anyCommand
) where

import Utils.Monad
import Data.Text
import Options.Applicative hiding (empty)
import Env
import Commands
import qualified Commands.Lexer as Lexer

pathArg :: Parser FilePath
pathArg =
    strArgument
        (  metavar "PATH"
        <> help "the path to the file"
        )

notesArg :: Parser Text
notesArg =
    strArgument
        (  metavar "TEXT"
        <> help "the notes associated to a file"
        )

progArg :: Parser ProgName
progArg =
    strArgument
        (  metavar "PROG"
        <> help "the program to use for opening a file"
        )

defaultMods :: String -> InfoMod a
defaultMods headerStr =
       fullDesc
    <> header headerStr

seeCmdParser :: ParserInfo Command
seeCmdParser =
    SEE <$> info (args <**> helper) (defaultMods "see - watch notes taken on file")
    where
        args :: Parser FilePath
        args = pathArg

takeCmdParser :: ParserInfo Command
takeCmdParser =
    uncurry TAKE <$> info (args <**> helper) (defaultMods "take - take notes on a file")
    where
        args :: Parser (FilePath, Text)
        args = pathArg `pairA` notesArg

editCmdParser :: ParserInfo Command
editCmdParser =
    uncurry EDIT <$> info (args <**> helper) (defaultMods "edit - change notes on a file")
    where
        args :: Parser (ProgName, FilePath)
        args = progArg `pairA` pathArg

performParser :: ParserInfo Command -> String -> [String] -> NotesKeeper Command
performParser parseCmd prog args =
    case execParserPure (prefs prefsMod) parseCmd args of
        Success cmd -> return cmd
        Failure failure -> do
            let (msg, _) = renderFailure failure prog
            throwError $ InvalidArgs msg
        CompletionInvoked _ -> throwError $ InvalidArgs "wtf" --TODO
    where
        prefsMod = showHelpOnError <> showHelpOnEmpty

seeCmd :: String -> [String] -> NotesKeeper Command
seeCmd = performParser seeCmdParser

takeCmd :: String -> [String] -> NotesKeeper Command
takeCmd = performParser takeCmdParser

editCmd :: String -> [String] -> NotesKeeper Command
editCmd = performParser editCmdParser

exeGetCommand :: String -> (String -> [String] -> NotesKeeper Command)
exeGetCommand prog
    | prog == Lexer.exeSee =
        seeCmd
    | prog == Lexer.exeTake =
        takeCmd
    --TODO: edit command
    | otherwise =
        \_ _ -> throwError $ InvalidCommand prog

replGetCommand :: String -> (String -> [String] -> NotesKeeper Command)
replGetCommand prog
    | prog == Lexer.replSee =
        seeCmd
    | prog == Lexer.replTake =
        takeCmd
    | prog == Lexer.replEdit =
        editCmd
    | otherwise =
        \_ _ -> throwError $ InvalidCommand prog

anyCommand :: String -> [String] -> NotesKeeper Command
anyCommand prog args = do
    mode <- getMode
    case mode of
        Repl -> replGetCommand prog prog args
        Exe -> exeGetCommand prog prog args
