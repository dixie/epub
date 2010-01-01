-- Open Packaging Format 2.0

module OPF (
        OPF(..),
        OPFEntry(..),
	mkOPFXML
) 
where
import Text.XML.Light

data OPF = OPF { 
              opfBookID    :: String,
              opfBookTitle :: String,
	      opfLang      :: String,
              opfEntries   :: [OPFEntry]
           } deriving Show

data OPFEntry = OPFEntry { 
	            opfeID        :: String,
		    opfeHRef      :: String,
		    opfeMediaType :: String
	        } deriving Show

opfDefaultLang = "en"
opfDefaultCreator = "Unknown"

mkNCXXML :: OPF -> String
mkNCXXML o = error "not implemented"


mkOPFXML :: OPF -> String
mkOPFXML o = ppTopElement packageT
	where
	   packageT= add_attrs packageA $ unode "package" nestedT
           packageA = [ (Attr (unqual "version") "2.0")
                       ,(Attr (unqual "unique-identifier") "BookId")
	               ,(Attr (unqual "xmlns")   "http://www.idpf.org/2007/opf") ]
           nestedT = [ metadataT, manifestT, spineT ]
           metadataT = add_attrs metadataA $ unode "metadata" [
                            unode "dc:title" (opfBookTitle o),
                            unode "dc:language" (opfLang o),
                            add_attrs [ (Attr (unqual "id") "BookId"), 
                                        (Attr (unqual "opf:scheme") "URI") 
                                      ] $ unode "dc:identifier" (opfBookID o)
                       ]
           metadataA = [ (Attr (unqual "xmlns:dc")  "http://purl.org/dc/elements/1.1/") 
                        ,(Attr (unqual "xmlns:opf") "http://www.idpf.org/2007/opf")
            	       ]
 	   manifestT = unode "manifest" (map manifestItemT (opfEntries o))
           manifestItemT (OPFEntry bID bHref bMedia) = add_attrs [ (Attr (unqual "id") bID) 
                                                                  ,(Attr (unqual "href") bHref)
                                                                  ,(Attr (unqual "media-type") bMedia) 
				                                 ] $ unode "item" ()
           spineT = add_attrs [ (Attr (unqual "toc") "ncx") ] $ unode "spine" (map spineItemT (opfEntries o))
           spineItemT (OPFEntry bID _ _) = add_attrs [ (Attr (unqual "idref") bID) ] $ unode "itemref" ()

