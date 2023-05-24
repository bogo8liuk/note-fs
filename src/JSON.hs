{-# LANGUAGE DeriveGeneric #-}

module JSON
    ( Notes
    , FilesNotes
    , decodingNotes
    , encodingNotes
) where

{- For json automatic encoding and decoding. -}
import GHC.Generics
{- For efficient note taking. -}
import Data.Text hiding (map)
import Data.ByteString.Lazy hiding (map)
import Data.Aeson

type Notes = Text

newtype FileData = FD (FilePath, Notes) deriving Generic

toKVPair :: FileData -> (FilePath, Notes)
toKVPair (FD kv) = kv

instance ToJSON FileData

instance FromJSON FileData

type WrappedNotes = [FileData]
type FilesNotes = [(FilePath, Notes)]

decondingWrappedNotes :: ByteString -> Either String WrappedNotes
decondingWrappedNotes = eitherDecode

decodingNotes :: ByteString -> Either String FilesNotes
decodingNotes bs =
    case decondingWrappedNotes bs of
        Left reason -> Left reason
        Right wNotes -> Right $ map toKVPair wNotes

encodingNotes :: FilesNotes -> ByteString
encodingNotes notes = encode $ map FD notes
