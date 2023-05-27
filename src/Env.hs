module Env
    ( NotesState(..)
    , NotesKeeper
    , NotesErr(..)
    , Mode(..)
    , throwError
    , retryOnError
    , catchError
    , handleErrorWith
    , getMode
    , getHistoryPath
    , runAsIO
    , performIO
    , display
    , display'
    , displayLn
    , displayLn'
    , ResumeOp
    , populateNotes
    , checkFileExists
    , commitChanges
    , commitIfEagerMode
    , canonicalizePath'
    , getNotesOf
    , removeEntry
    , overwriteEntry
    , editEntryWith
) where

import Utils.Fancy
import Utils.Monad
import Data.Text
import qualified Data.Text.IO as Text (putStrLn, putStr, writeFile, readFile)
import qualified Data.ByteString.Lazy as BS
import Data.Map.Strict as M hiding (update)
import Control.Monad.State
import Control.Monad.Except
import System.FilePath
import System.Directory
import System.Process (rawSystem)
import JSON

type NotesTable = Map FilePath Notes

data Mode =
    {- The effects of commands on the file-system aren't immediately performed, but they are delayed. -}
      Lazy
    {- The effects of commands on the file-system have to immediately performed. -}
    | Eager

data NotesState =
    NotesState
        {- The notes on files. -}
        { filesNotes :: NotesTable
        {- Is `filesNotes` field populated? This is kept in order to avoid useless IO operations on the fyle system
        (reading). Thus, the `filesNotes` population is lazy. -}
        , isPopulated :: Bool
        {- The file where the file notes are stored. -}
        , notesPath :: FilePath
        {- The repl history path. -}
        , historyPath :: FilePath
        {- The file where temporary notes can be taken. -}
        , editingPath :: FilePath
        {- The context where commands are executed. -}
        , mode :: Mode
        }

data NotesErr =
      FileNotFound FilePath
    | NotesNotFound FilePath
    | InvalidNotesPath FilePath
    | JSONErr Reason
    {- A command which doesn't exists. -}
    | InvalidCommand String
    {- A command which isn't used as it should be used. -}
    | InvalidArgs Usage

data ErrorType =
      User
    | Unexpected
    | Fatal

class ErrorLevel err where
    levelOf :: err -> ErrorType

instance ErrorLevel NotesErr where
    levelOf (FileNotFound _) = User
    levelOf (NotesNotFound _) = User
    levelOf (InvalidNotesPath _) = Fatal
    levelOf (JSONErr _) = Fatal
    levelOf (InvalidCommand _) = User
    levelOf (InvalidArgs _) = User

instance Show NotesErr where
    show (FileNotFound path) = "File " ++ path ++ " not found"
    show (NotesNotFound path) = "File " ++ path ++ " has no associated notes"
    show (InvalidNotesPath path) = "Invalid notes file " ++ path
    show (JSONErr reason) = "Conversion from/to json not successful due to: " ++ reason
    show (InvalidCommand cmd) = "Not a command: " ++ cmd
    show (InvalidArgs usage) = usage

type NotesKeeper = StateT NotesState (ExceptT NotesErr IO)

runAsIO :: NotesKeeper a -> NotesState -> IO (Either NotesErr a)
runAsIO op st = runExceptT $ evalStateT op st

performIO :: IO a -> NotesKeeper a
performIO = lift . liftIO

display :: String -> NotesKeeper ()
display = performIO . putStr

display' :: Text -> NotesKeeper ()
display' = performIO . Text.putStr

displayLn :: String -> NotesKeeper ()
displayLn = performIO . putStrLn

displayLn' :: Text -> NotesKeeper ()
displayLn' = performIO . Text.putStrLn

printErrorType :: NotesErr -> NotesKeeper ()
printErrorType err =
    case levelOf err of
        User -> doNothing
        Unexpected -> unexpected
        Fatal -> fatal
    where
        unexpected = displayLn "Unexpected error: "

        fatal = displayLn "FATAL error: "

type ResumeOp a = NotesKeeper a

{- It handles an error and, if it is not a fatal error, it resumes with another operation. -}
handleErrorWith :: ResumeOp a -> NotesErr -> NotesKeeper a
handleErrorWith resume err = do
    printErrorType err
    displayLn $ show err
    case levelOf err of
        User -> resume
        Unexpected -> resume
        {- Re-throw the error since it is fatal and nothing can be done. -}
        Fatal -> throwError err

retryOnError :: NotesKeeper a -> NotesKeeper a
retryOnError op =
    op `catchError` handleErrorWith (retryOnError op)

update :: (NotesState -> NotesState) -> NotesKeeper ()
update f = do
    env <- get
    put $ f env

getMode :: NotesKeeper Mode
getMode = gets mode

getHistoryPath :: NotesKeeper FilePath
getHistoryPath = gets historyPath

isNotesInMem :: NotesKeeper Bool
isNotesInMem = gets isPopulated

{- NB: this does not check that files notes is in memory. -}
rawGetNotes :: NotesKeeper NotesTable
rawGetNotes = gets filesNotes

ifNotesInMem :: (NotesTable -> NotesKeeper a) -> NotesKeeper a -> NotesKeeper a
ifNotesInMem doWith cont = do
    notesInMem <- isNotesInMem
    if notesInMem
    then do
        notes <- rawGetNotes
        doWith notes
    else
        cont

decodeNotes :: BS.ByteString -> NotesKeeper NotesTable
decodeNotes bs =
    if BS.null bs
    then return M.empty
    else
        case JSON.decodingNotes bs of
            Left reason -> throwError $ JSONErr reason
            Right notes -> return $ fromList notes

populateNotes :: NotesKeeper ()
populateNotes = do
    path <- gets notesPath
    existence <- performIO $ doesFileExist path
    if existence
    then do
        byteSrc <- performIO $ BS.readFile path
        notes <- decodeNotes byteSrc
        update $ \env -> env { filesNotes = notes, isPopulated = True }
    else
        throwError $ InvalidNotesPath path

checkFileExists :: FilePath -> NotesKeeper ()
checkFileExists path = do
    existence <- performIO $ doesFileExist path
    if existence
    then doNothing
    else throwError $ FileNotFound path

createNotesDirIfMissing :: NotesKeeper ()
createNotesDirIfMissing = do
    notesFile <- gets notesPath
    let notesDir = takeDirectory notesFile
    performIO $ createDirectoryIfMissing True notesDir

createHistoryDirIfMissing :: NotesKeeper ()
createHistoryDirIfMissing = do
    historyFile <- gets historyPath
    let historyDir = takeDirectory historyFile
    performIO $ createDirectoryIfMissing True historyDir

{- It writes the notes table to the notes file. Useful for lazy commits, namely not performing IO on the file-system until
it is really necessary. -}
commitChanges :: NotesKeeper ()
commitChanges = do
    createNotesDirIfMissing
    table <- getNotes
    let byteEntry = encodingNotes $ toList table
    notesFile <- gets notesPath
    performIO $ BS.writeFile notesFile byteEntry

commitIfEagerMode :: NotesKeeper ()
commitIfEagerMode = do
    mode <- getMode
    case mode of
        Eager -> commitChanges
        Lazy -> doNothing

canonicalizePath' :: FilePath -> NotesKeeper FilePath
canonicalizePath' = performIO . canonicalizePath

getNotes :: NotesKeeper NotesTable
getNotes =
    ifNotesInMem
        return
    `else'` do
        populateNotes
        rawGetNotes

getNotesOf :: FilePath -> NotesKeeper Text
getNotesOf path = do
    checkFileExists path
    table <- getNotes
    case M.lookup path table of
        Nothing -> throwError $ NotesNotFound path
        Just text -> return text

removeEntry :: FilePath -> NotesKeeper ()
removeEntry path = do
    table <- getNotes
    let table' = delete path table
    update $ \env -> env { filesNotes = table' }

overwriteEntry :: FilePath -> Text -> NotesKeeper ()
overwriteEntry path notes = do
    table <- getNotes
    let table' = insert path notes table
    update $ \env -> env { filesNotes = table' }

runOnEditingFile :: ProgName -> Text -> NotesKeeper Text
runOnEditingFile prog oldNotes = do
    editPath <- gets editingPath
    runProg editPath
    where
        runProg path = performIO $ do
            {- Writing the file with the old notes, in order to give the abstraction of editing notes. -}
            Text.writeFile path oldNotes
            rawSystem prog [path] --TODO: add arguments
            Text.readFile path

editEntryWith :: ProgName -> FilePath -> NotesKeeper ()
editEntryWith prog path = do
    oldNotes <- getNotesOf path
    notes <- runOnEditingFile prog oldNotes
    overwriteEntry path notes
