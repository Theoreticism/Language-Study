-- Hunt the Wumpus
-- Christopher Lee

module Map ( Map(Map), 
             rooms, 
             state,
    
             State(State), 
             wumpus, 
             player,
    
             Room(Room), 
             roomNumber, 
             connections, 
             getRoom,
             getAdjRooms,
             contains,
             getRandomPoint,
             getPossibleRooms,
             twoAdjRoom
             ) where

data Map = Map {rooms :: [Room], state :: [State]}
    
data Room = Room {roomNumber :: Int, connections :: [Int]}
    
type RoomNumber = Int   
type RoomPosition = [Int]       

instance Show Map where
    show m = foldl step [] (reverse (map show (rooms m)))                            -- quick note to self: map f xs applies f to each element of xs
        where step acc x = x ++ acc ++ "\n"                                          -- map :: (a -> b) -> [a] -> [b]

getRoom :: Map -> Int -> Room
getRoom a b = rooms a !! (b + 1)                                                     -- The !! (indexing operator) gets the nth element in a list, i.e. [1,2,3]!!1 gets you 2.    

getAdjRooms :: RoomNumber -> Map -> [RoomNumber]
getAdjRooms a b = connections (rooms b !! (a - 1))
                                          
instance Show Room where
    show room = show (roomNumber room) ++ ": " ++ show (connections room)
    
instance Eq Room where
    (Room x _) == (Room y _) = x == y
    
getAdjRooms :: RoomNumber -> Map -> [RoomNumber]
getAdjRooms a b = connections (rooms b !! (a - 1))                                   -- (!!) :: [a] -> Int -> a

getRandomPoint a = safeChoice (rooms a)

contains a b c = roomNumber a `elem` possibleRooms
    where possibleRooms = map roomNumber (getPossibleRooms c b)

getPossibleRooms a b = map (a `getRoom`) (connectedRooms b)

twoAdjRoom a b c = a `elem` getPossibleRooms c b

--generateMap :: RandomGen g => Int -> g -> (Map,g)
--generateMap _ gen = error "Not a valid move."
--generateMap 2 gen = (fst (random gen), fst (random gen))

generateMapPositions :: Int -> [RoomPosition]
generateMapPositions _ = error "Not a valid move."
generateMapPositions 2 = [[1],[2]]
generateMapPositions 20 = [[1,3,5],[3,7,10],[4,8,16],
                           [10,12,19],[6,14,16],[2,7,9]
                           [9,17,20],[8,11,14],[1,4,9]
                           [2,5,8],[7,18,20],[13,19,20]
                           [4,14,16],[3,5,12],[7,8,9]
                           [12,16,19],[5,7,11],[9,12,14]
                           [11,18,20],[7,15,19]]
