module Chapter1_Introduction where

import qualified Data.Text as T
import qualified Data.Map as M
import Utils


insight = do
    users <- readCsvWith parseUser "../data/chapter_1/users.csv"
    print users

    friendGraph <- readCsvWith parseGraphEdge "../data/chapter_1/friend_graph.csv"
    let g = toFriendshipsMap friendGraph
    print g


type UserId = Int
type FriendId = UserId

data User = User {
    uid  :: UserId,
    name :: T.Text
} deriving Show


-- Parses a user, e.g. ["1", "user1"] -> User { uid = 1, name = "user1" }
parseUser :: [T.Text] -> User
parseUser (s_uid:s_name:_) = User {
        uid = textToInt s_uid,
        name = s_name
    }

-- Parses a friend graph edge, e.g. ["1", "3"] -> (1, 3)
parseGraphEdge :: [T.Text] -> (UserId, FriendId)
parseGraphEdge (s_from:s_to:_) = (textToInt s_from, textToInt s_to)

-- Explicitly creates backward edges in a non-directional graph
-- e.g. [(0, 1), (1, 3)] -> [(0, 1), (1, 0), (1, 3), (3, 1)]
-- Precondition: a graph must not already contain both forward and backward edges
makeSymmetricEdges :: [(UserId, FriendId)] -> [(UserId, FriendId)]
makeSymmetricEdges [] = []
makeSymmetricEdges ((from, to):xs) = (from, to):(to, from):(makeSymmetricEdges xs)

toFriendshipsMap :: [(UserId, FriendId)] -> M.Map UserId [FriendId]
toFriendshipsMap = keyValuesToMap . makeSymmetricEdges
