module Chapter1_Introduction where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.List (sortBy)
import Data.Tuple (swap)
import CsvUtils
import Utils


insight = do
    users <- readCsvWith parseUser "data/chapter_1/users.csv"
    printd "Users:" users

    friendGraph <- readCsvWith parseGraphEdge "data/chapter_1/friend_graph.csv"
    let friendMap = keyValuesToMap . mirrorEdges $ friendGraph
    printd "Friendships:" friendMap

    interests <- readCsvWith parseInterest "data/chapter_1/interests.csv"
    let interestsByUserId = keyValuesToMap interests
    let userIdsByInterest = keyValuesToMap . map swap $ interests
    printd "Interests by user ID:" interestsByUserId
    printd "User IDs by interest:" userIdsByInterest

    printd "Average number of connections:" $ averageNumberOfConnections users friendMap

    printd "Users sorted by number of friends:" $ usersByNumberOfFriends users friendMap
    printd "Mutual friends of Chi (id = 3):" $ mutualFriends friendMap 3

    printd "Data scientists who like Java:" $ M.findWithDefault [] (T.pack "Java") userIdsByInterest


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

parseInterest :: [T.Text] -> (UserId, T.Text)
parseInterest (userId:interest:_) = (textToInt userId, interest) 

-- Explicitly creates backward edges in a non-directional graph
-- e.g. [(0, 1), (1, 3)] -> [(0, 1), (1, 0), (1, 3), (3, 1)]
-- Precondition: a graph must not already contain both forward and backward edges
mirrorEdges :: [(UserId, FriendId)] -> [(UserId, FriendId)]
mirrorEdges [] = []
mirrorEdges ((from,to):xs) = (from,to):(to,from):(mirrorEdges xs)

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

-- Finds users that have mutual friends with the target user
-- Output: Map { user_id: num_mutual_friends_with_target_user }
mutualFriends :: M.Map UserId [FriendId] -> UserId -> M.Map UserId Int
mutualFriends friendMap targetUserId = counter $ filter notMeNorMyFriend friendsOfMyFriends
    where
        friendsOfMyFriends = concat [friends friendMap f | f <- myFriends]
        notMeNorMyFriend userId = not ((userId `elem` myFriends) || (userId == targetUserId))
        myFriends = friends friendMap targetUserId
