module Chapter1_Introduction where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.List (sortBy)
import Utils


insight = do
    users <- readCsvWith parseUser "../data/chapter_1/users.csv"
    putStrLn $ concat ["Users:\n", show users, "\n"]

    friendGraph <- readCsvWith parseGraphEdge "../data/chapter_1/friend_graph.csv"
    let friendMap = toFriendshipsMap friendGraph
    putStrLn $ concat ["Friendships:\n", show friendMap, "\n"]

    putStrLn $ concat ["Average number of connections: ", show $ averageNumberOfConnections users friendMap, "\n"]

    putStrLn $ concat ["Users sorted by number of friends:\n", show $ usersByNumberOfFriends users friendMap, "\n"]


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

friends :: M.Map UserId [FriendId] -> UserId -> [FriendId]
friends friendMap userId = M.findWithDefault [] userId friendMap

numFriends :: M.Map UserId [FriendId] -> UserId -> Int
numFriends friendMap = length . friends friendMap

averageNumberOfConnections :: [User] -> M.Map UserId [FriendId] -> Double
averageNumberOfConnections users friendMap = totalConnections / numUsers
    where
        totalConnections = fromIntegral . sum . map (numFriends friendMap) . map uid $ users
        numUsers = fromIntegral . length $ users

usersByNumberOfFriends :: [User] -> M.Map UserId [FriendId] -> [(UserId, Int)]
usersByNumberOfFriends users friendMap = sortBy compareSecondDesc . zip userIds . map (numFriends friendMap) $ userIds
    where
        userIds = map uid users
        compareSecondDesc (_, a) (_, b) = compare b a
