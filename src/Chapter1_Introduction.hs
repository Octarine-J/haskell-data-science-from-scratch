module Chapter1_Introduction where

import qualified Data.Text as T
import Utils


insight = do
    users <- readCsvWith parseUser "../data/chapter_1/users.csv"
    print users

    friendGraph <- readCsvWith parseGraphEdge "../data/chapter_1/friend_graph.csv"
    print friendGraph


data User = User {
    uid  :: Int,
    name :: T.Text
} deriving Show


parseUser :: [T.Text] -> User
parseUser (s_uid:s_name:_) = User {
        uid = textToInt s_uid,
        name = s_name
    }

parseGraphEdge :: [T.Text] -> (Int, Int)
parseGraphEdge (s_from:s_to:_) = (textToInt s_from, textToInt s_to)
