import qualified Data.ByteString.Lazy as B
import Codec.EBook

main = do
  let book = emptyBook { 
     bookID = "http://localhost/pokus",
     bookAuthor = "Jozef Chroustal",
     bookTitle = "Macka a Pes"
  }
  print book
  print $ book2Str book
  B.writeFile "book.epub" (book2Str book) 
