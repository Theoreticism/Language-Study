module State (
              State(player, wumpus, gMap, State),
              start,
              gameWin,
              showPossibleRooms,
              makeMove,
              nose
             ) where

import Player
import Wumpus
import Map

data State = State {wumpus :: Wumpus, player :: Player, gMap :: Map}

start playerStart wumpusStart = State (Player playerstart 3 True) (Wumpus wumpusStart True)
    where gameMap = getMap 20

playerAndWumpus state = twoAdjRoom (Player.room (player state)) (Wumpus.room (wumpus state)) (gMap state)

getPossibleRooms state = Map.getPossibleRooms (gMap state) (Player.room (player state))

showPossibleRooms state = show (map Map.roomNumber (State.getPossibleRooms state))

makeMove _ _ _ = error "That is not a valid move! Shoot or move only."
makeMove "shoot" rn state = State aPlayer aWumpus (gMap state)
    where 
        aPlayer = Player.shoot (Map.getRoom (gMap state) (read rn :: Int)) (player state) (gMap state)
        aWumpus = Wumpus.ifWumpusIsShot (wumpus state) (Map.getRoom (gMap state) (read rn :: Int))
makeMove "move" rn state = State playerMove (wumpus state) (gMap state)
    where
        playerMove = if (Player.room aPlayer) == (Wumpus.room (wumpus state)) then (Player (Player.room aPlayer) (Player.numArrows aPlayer) False) else aPlayer
        aPlayer = Player.move (Map.getRoom (gMap state) (read roomNum :: Int)) (player state) (gMap state)
        
gameWin state
    | not (Wumpus.alive (wumpus state)) = 1
    | otherwise 0

