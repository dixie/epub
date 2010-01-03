-- Book Types definition

module Codec.EBook.Types (
   Book(..),
   BookItem(..),
   Metadata(..),
   emptyBook,
   addItem2Book,
   bookFiles,
   chapterItems
) 
where
import qualified Data.ByteString.Lazy as B

data Book = Book { 
               bookID     :: String,
               bookTitle  :: String,
               bookAuthor :: String, 
               bookLang   :: String,
               bookItems  :: [BookItem]
            } deriving (Show, Eq, Ord)

data BookItem = BookItem {
               itemID        :: String,
               itemFileName  :: FilePath,
               itemContent   :: B.ByteString,
               itemMediaType :: String,
               itemMetadata  :: Maybe Metadata
            } deriving (Show, Eq, Ord)

data Metadata = ChapterMetadata {
                chapterTitle :: String
            } deriving (Show, Eq, Ord)

emptyBook :: Book
emptyBook = Book "NO ID" "NO TITLE" "NO AUTHOR" "en" []

bookFiles :: Book -> [(FilePath, B.ByteString)]
bookFiles book = map (\x -> (itemFileName x, itemContent x)) (bookItems book)

chapterItems :: [BookItem] -> [BookItem]
chapterItems = filter (isChapter)
   where
      isChapter (BookItem _ _ _ _ Nothing ) = False
      isChapter _ = True

addItem2Book :: Book -> BookItem -> Book
addItem2Book book item = book { bookItems = newItems }
       where
          newItems = (bookItems book) ++ [item]
