module Utils (
        readCsv, 
        readCsvWith,
        textToInt,
        keyValuesToMap,
        counter
    ) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import Data.List (groupBy, sort)

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

-- Converts a list of key/values into a map, aggregating values
-- e.g. [(1, "one"), (2, "two"), (1, "uno")] -> Map {1: ["one", "uno"], 2: ["two"]} 
keyValuesToMap :: (Eq k, Ord k, Ord v) => [(k, v)] -> M.Map k [v]
keyValuesToMap = M.fromList . groupValues
    where
        groupValues :: (Eq k, Ord k, Ord v) => [(k, v)] -> [(k, [v])]
        groupValues = map foldValues . groupBy (\(x, _) (y, _) -> x == y) . sort 

        -- Combines values of the same key into a list
        -- e.g. [(1, 2), (1, 4), (1, 5)] -> (1, [2, 4, 5]) 
        -- Precondition: the key must be the same for all values in the input list
        foldValues :: (Eq k, Ord k, Ord v) => [(k, v)] -> (k, [v])
        foldValues xs = foldr accumulateSameKeyValues (key, []) xs
            where
                key = fst . head $ xs

                accumulateSameKeyValues :: (Eq k, Ord k, Ord v) => (k, v) -> (k, [v]) -> (k, [v])
                accumulateSameKeyValues (key1, value) (key2, values)
                    | key1 == key2 = (key1, value:values)
                    | otherwise = error "Cannot group values for different keys."

-- Counts number of same elements in a list, e.g.
-- [1, 3, 1, 5, 3, 1] -> Map {1: 3, 3: 2, 5: 1}
counter :: (Eq k, Ord k) => [k] -> M.Map k Int
counter = M.fromList . count
    where
        count :: (Eq k, Ord k) => [k] -> [(k, Int)]
        count = map (\xs -> (xs !! 0, length xs)) . groupBy (==) . sort
