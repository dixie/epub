-- Open Packaging Format 2.0

module Codec.EBook.OPF (
	ncxXML,
        opfXML,
        opfFiles
) 
where
import Data.Maybe (fromJust)
import Text.XML.Light
import Codec.EBook.Types
import qualified Data.ByteString.Lazy as B

opfDefaultLang = "en"
opfDefaultCreator = "Unknown"

opfFiles :: Book -> FilePath -> [(FilePath, B.ByteString)] 
opfFiles book name = let ncxXMLFile = (ncxFileName,ncxXML book)
                         opfXMLFile = (name,opfXML book ncxFileName)
                         ncxFileName = "book.ncx"
                     in [opfXMLFile, ncxXMLFile]

ncxXML :: Book -> B.ByteString
ncxXML o = str2bstr $ ppTopElement packageT
	where
	   packageT= add_attrs packageA $ unode "ncx" nestedT
           packageA = [ Attr (unqual "version") "2005-1"
                       ,Attr (unqual "xml:lang") "en"
	               ,Attr (unqual "xmlns")   "http://www.daisy.org/z3986/2005/ncx/" ]
           nestedT = [ headT, docTitleT, docAuthorT, navMapT ]
           headT = unode "head" $ map (\(n,v) -> add_attrs [ Attr (unqual "name") n,Attr (unqual "content") v] $ unode "meta" ()) metaVals
           metaVals = [ ("dtb:uid",(bookID o))
                       ,("dtb:depth","1")
                       ,("dtb:totalPageCount","0") 
                       ,("dtb:maxPageNumber","0") ]
           docTitleT = unode "docTitle" $ unode "text" (bookTitle o)
           docAuthorT = unode "docAuthor" $ unode "text" (bookAuthor o)
           numChapterEnt = zip (chapterItems $ bookItems o) [1..]
           navMapT = unode "navMap" (map navPointT numChapterEnt)
           navPointT (e,i) = add_attrs [ Attr (unqual "class") "chapter" 
                                        ,Attr (unqual "id") (itemID e)
                                        ,Attr (unqual "playOrder") (show i) 
                                      ] $ unode "navPoint" [ unode "navLabel" $ unode "text" (chapterTitle (fromJust $ itemMetadata e))
                                                            ,add_attr (Attr (unqual "src") (itemFileName e) ) $ unode "content" ()
					  	           ]

opfXML :: Book -> FilePath -> B.ByteString
opfXML o xn = str2bstr $ ppTopElement packageT
	where
	   packageT= add_attrs packageA $ unode "package" nestedT
           packageA = [ Attr (unqual "version") "2.0"
                       ,Attr (unqual "unique-identifier") "BookId"
	               ,Attr (unqual "xmlns")   "http://www.idpf.org/2007/opf" ]
           nestedT = [ metadataT, manifestT, spineT ]
           metadataT = add_attrs metadataA $ unode "metadata" [
                            unode "dc:title" (bookTitle o),
                            unode "dc:language" (bookLang o),
                            add_attrs [ Attr (unqual "id") "BookId" 
                                       ,Attr (unqual "opf:scheme") "URI" 
                                      ] $ unode "dc:identifier" (bookID o)
                       ]
           metadataA = [ Attr (unqual "xmlns:dc")  "http://purl.org/dc/elements/1.1/" 
                        ,Attr (unqual "xmlns:opf") "http://www.idpf.org/2007/opf"
            	       ]
 	   manifestT = unode "manifest" $ map manifestItemT (bookItems o) ++ [ncxItem]
           manifestItemT i = add_attrs [ Attr (unqual "id") (itemID i) 
                                        ,Attr (unqual "href") (itemFileName i)
                                        ,Attr (unqual "media-type") (itemMediaType i) 
                                       ] $ unode "item" ()
           ncxItem = add_attrs [ Attr (unqual "id") "ncx" 
                                ,Attr (unqual "href") xn
                                ,Attr (unqual "media-type") "application/x-dtbncx+xml" 
                                       ] $ unode "item" ()
           spineT = add_attrs [ Attr (unqual "toc") "ncx" ] $ unode "spine" (map spineItemT (bookItems o))
           spineItemT i = add_attrs [ Attr (unqual "idref") (itemID i) ] $ unode "itemref" ()

