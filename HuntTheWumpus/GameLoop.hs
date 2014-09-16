import State
import Map
import Wumpus
import Player

gameLoop state = do
    putStrLn (State.playerSmell state)
    putStrLn "(move/shoot)"
    choice <- getLine
    putStrLn ("Room " ++ State.showPossibleRooms state)
    newRoom <- getLine
    
main = do
    let myGameMap = Map.getMap 20
    playerStart <- Map.getRandomPoint myGameMap
    wumpusStart <- Map.getRandomPoint myGameMap
    let state = State.start playerStart wumpusStart myGameMap
    gameLoop state
    
-- (Incomplete)