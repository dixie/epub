-- Book Types definition

module Codec.EBook.Types (
   Book(..),
   BookItem(..),
   Metadata(..),
   emptyBook,
   addItem2Book
) 
where

data Book = Book { 
               bookID     :: String,
               bookTitle  :: String,
               bookAuthor :: String, 
               bookLang   :: String,
               bookItems  :: [BookItem]
            } deriving (Show, Ord, Eq)

data BookItem = BookItem {
               itemFileName  :: String,
               itemContent   :: String,
               itemMediaType :: String,
               itemMetadata  :: Maybe Metadata
            } deriving (Show, Ord, Eq)

data Metadata = ChapterMetadata {
                chapterTitle :: String
            } deriving (Show, Ord, Eq)

emptyBook :: Book
emptyBook = Book "NO ID" "NO TITLE" "NO AUTHOR" "en" []

addItem2Book :: Book -> BookItem -> Book
addItem2Book book item = book { bookItems = newItems }
       where
          newItems = (bookItems book) ++ [item]
