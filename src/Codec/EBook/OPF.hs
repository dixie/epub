-- Open Packaging Format 2.0

module Codec.EBook.OPF (
        OPF(..),
        OPFEntry(..),
	mkOPFXML
) 
where
import Text.XML.Light

data OPF = OPF { 
              opfBookID     :: String,
              opfBookTitle  :: String,
              opfBookAuthor :: String,
	      opfLang       :: String,
              opfEntries    :: [OPFEntry]
           } deriving Show

data OPFEntry = OPFEntry { 
	            opfeID        :: String,
		    opfeHRef      :: String,
		    opfeMediaType :: String }
              | OPFEntryChapter { 
	            opfeID            :: String,
		    opfeHRef          :: String,
		    opfeMediaType     :: String,
                    opfeChapterName   :: String } deriving Show

opfDefaultLang = "en"
opfDefaultCreator = "Unknown"

chapterEntries :: [OPFEntry] -> [OPFEntry]
chapterEntries = filter (isChapter)
   where
      isChapter (OPFEntryChapter _ _ _ _) = True
      isChapter _ = False

mkNCXXML :: OPF -> String
mkNCXXML o = ppTopElement packageT
	where
	   packageT= add_attrs packageA $ unode "ncx" nestedT
           packageA = [ (Attr (unqual "version") "2005-1")
                       ,(Attr (unqual "xml:lang") "en")
	               ,(Attr (unqual "xmlns")   "http://www.daisy.org/z3986/2005/ncx/") ]
           nestedT = [ headT, docTitleT, docAuthorT, navMapT ]
           headT = unode "head" $ map (\(n,v) -> add_attrs [ (Attr (unqual "name") n),(Attr (unqual "content") v)] $ unode "meta" ()) metaVals
           metaVals = [ ("dtb:uid",(opfBookID o))
                       ,("dtb:depth","1")
                       ,("dtb:totalPageCount","0") 
                       ,("dtb:maxPageNumber","0") ]
           docTitleT = unode "docTitle" $ unode "text" (opfBookTitle o)
           docAuthorT = unode "docAuthor" $ unode "text" (opfBookAuthor o)
           numChapterEnt = zip (chapterEntries $ opfEntries o) [1..]
           navMapT = unode "navMap" (map navPointT numChapterEnt)
           navPointT (e,i) = add_attrs [ (Attr (unqual "class") "chapter") 
                                        ,(Attr (unqual "id") (opfeID e))
                                        ,(Attr (unqual "playOrder") (show i)) 
                                      ] $ unode "navPoint" [ unode "navLabel" $ unode "text" (opfeChapterName e)
                                                            ,add_attr (Attr (unqual "src") (opfeHRef e) ) $ unode "content" ()
					  	           ]

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

