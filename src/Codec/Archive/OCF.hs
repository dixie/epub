module OCF where

import Codec.Archive.Zip

-- | Represents OCF Container Format
data OCF = OCF { ocMetadata :: String,
                 ocRootfiles :: [RootFile],
                 ocFiles :: [Files] } deriving Show

-- | Represent one root file within OCF
data RootFile = RootFile { rfPath :: String, rfMediaType :: String } deriving Show

-- | Create ZIP archive with represents OCF

writeOCF :: FilePath -> OCF -> IO ()
writeOCF f o = do
