import qualified Data.ByteString.Lazy as B
import Codec.EBook

main = do
  let book = emptyBook { 
     bookID = "http://localhost/pokus",
     bookAuthor = "Jozef Chroustal",
     bookTitle = "Macka a Pes"
  }
  print book
  outdata <- book2Str' book
  print outdata
  B.writeFile "book.epub" outdata
