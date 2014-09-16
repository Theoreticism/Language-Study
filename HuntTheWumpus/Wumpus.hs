module Wumpus ( Wumpus(Wumpus),
                room,
                alive,
                ifWumpusIsShot,
              ) where
              
import Map
import Data.Random
import Data.Random.Extras

data Wumpus = Wumpus {room :: Room, alive :: Bool}

move newLoc wumpus gMap
    | twoAdjRoom newLoc (room wumpus) gMap = Wumpus newLoc
    | otherwise = error "Invalid"

ifWumpusIsShot wumpus currLoc
    | room wumpus == currLoc = Wumpus currLoc False
    | otherwise = wumpus
    
-- getWumpusMove 