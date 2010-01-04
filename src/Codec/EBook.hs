-- EBook Types definition

module Codec.EBook (
    module Codec.EBook.Types,
    module Codec.EBook.OPF,
    module Codec.EBook.OCF,
    book2Str,
    book2Arch
) 
where

import Codec.Archive.Zip
import Codec.EBook.Types
import Codec.EBook.OPF
import Codec.EBook.OCF
import qualified Data.ByteString.Lazy as B

book2Str :: Book -> B.ByteString
book2Str book = fromArchive (book2Arch book)

book2Arch :: Book -> Archive
book2Arch book = let ncxXMLFile = ("book.ncx",ncxXML book)
                     opfXMLFile = ("book.opf",opfXML book)
                     conXMLFile = containerXMLFile' "book.opf"
                     mimeFile   = mimetypeFile
                     contFiles  = bookFiles book
                     allFiles   = mimeFile:ncxXMLFile:conXMLFile:opfXMLFile:contFiles
                     entries    = map (\(n,c) -> toEntry n 10000000 c) allFiles
                     arch       = foldl (\a e -> addEntryToArchive e a) emptyArchive entries
                 in  arch 
