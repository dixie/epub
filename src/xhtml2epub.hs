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
          items <- mapM loadItems $ zip fileNames [1..]
          let bookFull = foldl' addItem2Book book items
          let epubFName = nameOfBook <.> "epub"
          outdata <- book2Bin' bookFull
          B.writeFile epubFName  outdata
          putStrLn $ epubFName ++ " constructed."
     _ -> error "Usage: xhtml2epub <name of book> <xhtml file1> [<xhtml file2>,...]"

-- | Constructs a book item from a given filename and item index. The
-- index is used to generate a unique item identifier.
loadItems :: (FilePath,Int) -> IO BookItem
loadItems (p,i) = do
   c <- B.readFile p
   return (BookItem iid np c opsMediatype (Just (ChapterMetadata np)))
   where
      np = normalise p
      iid = takeBaseName p ++ "-" ++ show i
