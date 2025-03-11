module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Tarefa5
import Tarefa6

data Manager = Manager {state :: State, pid :: Int, step :: Int, before :: Integer, delta :: Integer, delay :: Integer}

managerSample :: Manager
managerSample = Manager initialState 0 0 0 0 defaultDelayTime

initialState :: State
initialState = State mazeOne playerList 1

playerList = [player1,player2,player3,player4,player5]
player1 = Pacman(PacState (0,(1,1),1,Null,0,2) 0 Open Normal)
player2 = Ghost(GhoState (1,(5,18),1,Null,0,0) Alive)
player3 = Ghost(GhoState (2,(5,19),1,Null,0,0) Alive)
player4 = Ghost(GhoState (3,(5,21),1,Null,0,0) Alive)
player5 = Ghost(GhoState (4,(5,22),1,Null,0,0) Alive)

mazeOne = 
    [
        [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
        [Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Wall,Wall,Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Wall],
        [Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall,Food Little,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Food Little,Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall],
        [Wall,Food Little,Wall,Food Big,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Wall,Food Little,Food Little,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Food Little,Food Little,Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall,Food Little,Wall],
        [Wall,Food Little,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Food Little,Wall,Empty,Wall,Wall,Wall,Empty,Empty,Empty,Wall,Wall,Wall,Empty,Wall,Food Little,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Food Little,Wall],
        [Empty,Food Little,Wall,Wall,Food Little,Wall,Empty,Empty,Empty,Wall,Food Little,Wall,Wall,Food Little,Wall,Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty,Wall,Food Little,Wall,Wall,Food Little,Wall,Empty,Empty,Empty,Wall,Food Little,Wall,Wall,Food Little,Empty],
        [Empty,Food Little,Wall,Wall,Food Little,Wall,Empty,Empty,Empty,Wall,Food Little,Wall,Wall,Food Little,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Food Little,Wall,Wall,Food Little,Wall,Empty,Empty,Empty,Wall,Food Little,Wall,Wall,Food Little,Empty],
        [Wall,Food Little,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Food Little,Wall,Food Little,Food Little,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Food Little,Food Little,Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Food Little,Wall],
        [Wall,Food Little,Wall,Food Big,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall,Food Little,Wall],
        [Wall,Food Little,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Food Little,Wall],
        [Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Wall,Wall,Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Wall,Wall,Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Wall],
        [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]
    ]

defaultDelayTime :: Integer
defaultDelayTime = 250

updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer KeyLeftArrow man  = Manager (beforePlay 0 L (state man)) (pid man) (step man) (before man) (delta man) (delay man)
updateControlledPlayer KeyRightArrow man = Manager (beforePlay 0 R (state man)) (pid man) (step man) (before man) (delta man) (delay man)
updateControlledPlayer KeyUpArrow man    = Manager (beforePlay 0 U (state man)) (pid man) (step man) (before man) (delta man) (delay man)
updateControlledPlayer KeyDownArrow man  = Manager (beforePlay 0 D (state man)) (pid man) (step man) (before man) (delta man) (delay man)

currentTime :: IO Integer
currentTime = fmap (round . (* 1000)) getPOSIXTime

updateTime :: Integer -> Manager -> Manager
updateTime now man = Manager (state man) (pid man) (step man) (before man) (abs(now - before man)) (delay man)

nextFrame :: Integer -> Manager -> Manager
nextFrame now man = Manager (passTime (step man) (state man)) (pid man) (step man + 1) now 0 (delay man)

updateScreen :: Window -> Manager -> Curses ()
updateScreen w man =
    do
        updateWindow w $ do
            clear
            moveCursor 0 0
            drawString $ show (state man)
        render

loop :: Window -> Manager -> Curses ()
loop w man@(Manager state pid step before delta delay) = do
    color_schema <- newColorID ColorBlue ColorWhite 10
    now <- liftIO currentTime
    updateScreen w man
    if delta > delay then
        loop w (nextFrame now man)
    else
        do
        ev <- getEvent w (Just 0)
        case ev of
            Nothing -> loop w (updateTime now man)
            Just (EventSpecialKey arrow) -> loop w (updateControlledPlayer arrow man)
            Just ev' ->
                if ev' == EventCharacter 'r' then
                    loop w managerSample
                else    
                    if ev' == EventCharacter 'q' then
                        return ()
                    else
                        loop w (updateTime now man)

main :: IO ()
main = 
    runCurses $ do
        setEcho False
        setCursorMode CursorInvisible
        w <- defaultWindow
        loop w managerSample