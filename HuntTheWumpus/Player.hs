module Player ( Player(Player),
                ammo,
                location,
                alive,
                move,
                shoot,
                smell
              ) where

import Map

data Player = Player {ammo :: Int, location :: Room, alive :: Bool}

-- filter returns a list of arguments from the second argument fulfilling the first argument condition

smell :: Player -> Map -> String
smell gameMap player wumpus
    | filter (location wumpus) adjRooms = "Something nearby smells putrid..."       
    | otherwise = ""
    where
        adjRooms = getAdjacentRooms (location player) gameMap
        
move :: Player -> Map -> Player
move newLoc player gMap
    | twoAdjRoom newLoc (room player) gMap = Player newLoc (ammo player) True
    | otherwise = error "That is not a valid location to move to."

shoot :: Player -> Map -> Int -> (Player,Bool)
shoot loc player gMap
    | ammo player <= 0 = error "Out of ammo!"
    | twoAdjRoom loc (room player) gMap = Player (room player) (ammo player - 1) True
    | otherwise = error "That is not a valid location to shoot into."