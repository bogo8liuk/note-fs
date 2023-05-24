module Commands.Parsing
    ( anyCmd
) where

import Utils.Fancy
import Utils.Monad
import Data.Text
import Options.Applicative hiding (empty)
import Env
import Commands

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

takeNoteCmdParser :: ParserInfo Command
takeNoteCmdParser =
    uncurry TAKE_NOTE <$> info (args <**> helper) (defaultMods "take - take notes on a file")
    where
        args :: Parser (FilePath, Text)
        args = pathArg `pairA` notesArg

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

takeNoteCmd :: String -> [String] -> NotesKeeper Command
takeNoteCmd = performParser takeNoteCmdParser

getCommandFrom :: String -> (String -> [String] -> NotesKeeper Command)
getCommandFrom "see" = seeCmd
getCommandFrom "take" = takeNoteCmd
getCommandFrom prog = \_ _ -> throwError $ InvalidCommand prog

anyCmd :: String -> [String] -> NotesKeeper Command
anyCmd prog = getCommandFrom prog prog
