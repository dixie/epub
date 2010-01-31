import qualified Data.ByteString.Lazy as B
import Codec.EBook
import System.Environment (getArgs)
import System.FilePath
import Data.List (foldl')

main = do
  args <- getArgs
  case args of 
     (nameOfBook:fileNames) -> do 
          let book = emptyBook { 
                bookID = "http://localhost/"++nameOfBook,
                bookAuthor = "xhtml2epub",
                bookTitle = nameOfBook
          }
          items <- mapM loadItems fileNames
          let bookFull = foldl' addItem2Book book items
          let epubFName = nameOfBook++".epub"
          outdata <- book2Bin' bookFull
          B.writeFile epubFName  outdata
          putStrLn $ epubFName ++ " constructed."
     _ -> error "Usage: xhtml2epub <name of book> <xhtml file1> [<xhtml file2>,...]"

loadItems :: FilePath -> IO BookItem
loadItems p = do
   c <- B.readFile p
   return (BookItem ("http://localhost/"++np) np c opsMediatype (Just (ChapterMetadata np))) 
   where
      np = normalise p
