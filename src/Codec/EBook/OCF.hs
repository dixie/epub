-- | Open Container Format 1.0

module Codec.EBook.OCF(
	defaultMediatype,
	defaultMimetype,
	containerXMLFile,
        containerXMLFile',
        mimetypeFile,
       )
where

import Text.XML.Light
import Codec.Archive.Zip
import Codec.Binary.UTF8.String as U
import qualified Data.ByteString.Lazy as B

defaultMediatype = "application/oebps-package+xml"
defaultMimetype = "application/epub+zip"

containerXMLFile' :: FilePath -> (FilePath, B.ByteString)
containerXMLFile' p = containerXMLFile p defaultMediatype

mimetypeFile :: (FilePath, B.ByteString)
mimetypeFile = ("mimetype", B.pack $ U.encode $ defaultMimetype)

containerXMLFile :: FilePath -> String -> (FilePath, B.ByteString)
containerXMLFile p m = ("META-INF/container.xml", B.pack $ U.encode $ ppTopElement contTag)
	where
	   contTag = add_attrs contAttrs $ unode "container" rootfilesTag
           contAttrs = [ (Attr (unqual "version") "1.0")
			,(Attr (unqual "xmlns")   "urn:oasis:names:tc:opendocument:xmlns:container") ] 
           rootfilesTag = unode "rootfiles" (rootfileTag p m)
           rootfileTag p m = add_attrs (rfAttrs p m) $ unode "rootfile" ()
           rfAttrs p m = [ (Attr (unqual "full-path") p)
			  ,(Attr (unqual "media-type")  m) ] 
