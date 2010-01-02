-- EBook Types definition

module Codec.EBook (
    module Codec.EBook.Types,
    module Codec.EBook.OPF,
    module Codec.EBook.OCF,
    book2ByteString,
    book2OCF,
    book2OPF
) 
where

import Codec.EBook.Types
import Codec.EBook.OPF
import Codec.EBook.OCF
import qualified Data.ByteString.Lazy.Internal as B

book2ByteString :: Book -> B.ByteString
book2ByteString book = undefined

book2OCF :: Book -> OCF
book2OCF book = undefined

book2OPF :: Book -> OPF
book2OPF book = undefined
