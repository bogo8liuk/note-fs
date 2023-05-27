module Commands.Parsing
    ( seeCmd
    , takeCmd
    , editCmd
    , exitCmd
    , anyCommand
) where

import Utils.Fancy (ProgName)
import Utils.Monad
import Data.Text
import Options.Applicative as Opt
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

noArgs :: Parser ()
noArgs = pure ()

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

exitCmdParser :: ParserInfo Command
exitCmdParser =
    EXIT <$ info (noArgs <**> helper) (defaultMods "exit - exit the note-fs program")

commitCmdParser :: ParserInfo Command
commitCmdParser =
    COMMIT <$ info (noArgs <**> helper) (defaultMods "commit - it brings the performed changes on the file system")

loadCmdParser :: ParserInfo Command
loadCmdParser =
    LOAD <$ info (noArgs <**> helper) (defaultMods "load - it loads the cache with the data in the file system")

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

exitCmd :: String -> [String] -> NotesKeeper Command
exitCmd = performParser exitCmdParser

commitCmd :: String -> [String] -> NotesKeeper Command
commitCmd = performParser commitCmdParser

loadCmd :: String -> [String] -> NotesKeeper Command
loadCmd = performParser loadCmdParser

exeGetCommand :: String -> (String -> [String] -> NotesKeeper Command)
exeGetCommand prog
    | prog == Lexer.exeSee =
        seeCmd
    | prog == Lexer.exeTake =
        takeCmd
    | prog == Lexer.exeEdit =
        editCmd
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
    | prog == Lexer.replExit =
        exitCmd
    | prog == Lexer.replCommit =
        commitCmd
    | prog == Lexer.replLoad =
        loadCmd
    | otherwise =
        \_ _ -> throwError $ InvalidCommand prog

anyCommand :: String -> [String] -> NotesKeeper Command
anyCommand prog args = do
    mode <- getMode
    case mode of
        Lazy -> replGetCommand prog prog args
        Eager -> exeGetCommand prog prog args
