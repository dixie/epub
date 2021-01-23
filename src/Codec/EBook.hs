-- EBook Types definition

module Codec.EBook (
    module Codec.EBook.Types,
    module Codec.EBook.OPF,
    module Codec.EBook.OCF,
    book2Bin,
    book2Bin',
    book2Arch',
    book2Arch,
    opsMediatype
) 
where

import System.Time 
import Codec.Archive.Zip
import Codec.EBook.Types
import Codec.EBook.OPF
import Codec.EBook.OCF
import qualified Data.ByteString.Lazy as B

-- | EPUB media type
opsMediatype :: String
opsMediatype = "application/xhtml+xml"

-- | Serialize Book to binary EPUB Format
book2Bin :: Book -> Integer -> B.ByteString
book2Bin book = fromArchive . book2Arch book

-- | Serialize Book to binary EPUB Format with current time
book2Bin' :: Book -> IO B.ByteString
book2Bin' book = do 
          (TOD t _) <- getClockTime 
          return $ fromArchive (book2Arch book t)

-- | Serialize Book to ZIP Archive with current time
book2Arch' :: Book -> IO Archive
book2Arch' book = do
          (TOD t _) <- getClockTime 
          return $ book2Arch book t

-- | Serialize Book to ZIP Archive
book2Arch :: Book -> Integer -> Archive
book2Arch book t = let conXMLFile = containerXMLFile' opfFileName
                       opfFileName = "book.opf"
                       mimeFile   = mimetypeFile
                       contFiles  = bookFiles book
                       allFiles   = conXMLFile:contFiles ++ opfFiles book opfFileName ++ [mimeFile]
                       entries    = map (\(n,c) -> toEntry n t c) allFiles
                       arch       = foldl (\a e -> addEntryToArchive e a) emptyArchive entries
                   in  arch 
