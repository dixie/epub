-- | Open Container Format 1.0

module OCF(
	OCF(..),
	RootItem(..),
	emptyOCF,
	addRootItem,
	addFile,
	defaultMediatype,
	defaultMimetype
       )
where

import Text.XML.Light
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy.Internal as B

defaultMediatype = "application/oebps-package+xml"
defaultMimetype = "application/epub+zip"

-- | Represents OCF Container Format
data OCF = OCF { ocfMimetype  :: String,
                 ocfRootItems :: [RootItem],
                 ocfEntries   :: [Entry] } deriving Show

-- | Represent one root file within OCF
data RootItem = RootItem { rfPath :: String, rfMediaType :: String } deriving Show

-- | Create empty OCF Container with defaultMimetype
emptyOCF :: OCF
emptyOCF = OCF defaultMimetype [] []

-- | Add Root Item into OCF
addRootItem :: OCF -> RootItem -> OCF
addRootItem o i = o { ocfRootItems = its } 
	where
	   its = i:(ocfRootItems o) 

-- | Put file within container
addFile :: OCF -> FilePath -> B.ByteString -> OCF
addFile o f c = o { ocfEntries = ets }
       where
           ets = (toEntry f 0 c):(ocfEntries o)

mkContainerXML :: OCF -> String
mkContainerXML o = ppTopElement contTag
	where
	   contTag = add_attrs contAttrs $ unode "container" rootfilesTag
           contAttrs = [ (Attr (unqual "version") "1.0")
			,(Attr (unqual "xmlns")   "urn:oasis:names:tc:opendocument:xmlns:container") ] 
           rootfilesTag = unode "rootfiles" (map rootfileTag (ocfRootItems o))
           rootfileTag (RootItem p m) = add_attrs (rfAttrs p m) $ unode "rootfile" ()
           rfAttrs p m = [ (Attr (unqual "full-path") p)
			  ,(Attr (unqual "media-type")  m) ] 
