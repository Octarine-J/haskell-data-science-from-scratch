module Utils (
        readCsv, 
        readCsvWith,
        textToInt
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TextIO

csvSeparator :: T.Text
csvSeparator = T.pack ","

readCsvWith :: ([T.Text] -> a) -> String -> IO [a]
readCsvWith f filename = do
    csv <- readCsv filename
    return . map f $ csv

readCsv :: String -> IO [[T.Text]]
readCsv filename = do
    content <- TextIO.readFile filename
    return $ parseCsv content

parseCsv :: T.Text -> [[T.Text]]
parseCsv = map (T.splitOn csvSeparator) . T.lines

textToInt :: T.Text -> Int
textToInt = read . T.unpack
