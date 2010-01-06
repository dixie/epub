import qualified Data.ByteString.Lazy as B
import Codec.EBook
import System.Environment (getArgs)
import System.FilePath
import Data.List (foldl')

main = do
  fileNames <- getArgs
  let book = emptyBook { 
     bookID = "http://localhost/pokus",
     bookAuthor = "Jozef Chroustal",
     bookTitle = "Macka a Pes"
  }
  items <- mapM loadItems fileNames
  let bookFull = foldl' addItem2Book book items
  print bookFull
  outdata <- book2Str' bookFull
  print outdata
  B.writeFile "book.epub" outdata

loadItems :: FilePath -> IO BookItem
loadItems p = do
   c <- B.readFile p
   return (BookItem ("http://localhost/"++np) np c opsMediatype (Just (ChapterMetadata np))) 
   where
      np = normalise p
