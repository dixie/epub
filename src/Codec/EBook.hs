-- EBook Types definition

module Codec.EBook (
    module Codec.EBook.Types,
    module Codec.EBook.OPF,
    module Codec.EBook.OCF,
    book2Str,
    book2Str',
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

opsMediatype = "application/xhtml+xml"

book2Str :: Book -> Integer -> B.ByteString
book2Str book t = fromArchive (book2Arch book t) 

book2Str' :: Book -> IO B.ByteString
book2Str' book = do 
          (TOD t _) <- getClockTime 
          return $ fromArchive (book2Arch book t)

book2Arch' :: Book -> IO Archive
book2Arch' book = do
          (TOD t _) <- getClockTime 
          return $ book2Arch book t

book2Arch :: Book -> Integer -> Archive
book2Arch book t = let conXMLFile = containerXMLFile' opfFileName
                       opfFileName = "book.opf"
                       mimeFile   = mimetypeFile
                       contFiles  = bookFiles book
                       allFiles   = mimeFile:conXMLFile:contFiles ++ (opfFiles book opfFileName)
                       entries    = map (\(n,c) -> toEntry n t c) allFiles
                       arch       = foldl (\a e -> addEntryToArchive e a) emptyArchive entries
                   in  arch 
