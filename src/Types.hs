module Types where

import Data.List

data State = State {maze :: Maze, playersState :: [Player], level :: Int}

type Maze = [Corridor]
type Corridor = [Piece]
data Piece =  Food FoodType | PacPlayer Player | Empty | Wall deriving (Eq)
data Player =  Pacman PacState | Ghost GhoState deriving (Eq)

data Orientation = L | R | U | D | Null deriving (Eq,Show)
data PacState= PacState {pacState :: PlayerState, timeMega :: Double, openClosed :: Mouth, pacmanMode :: PacMode} deriving Eq
data GhoState= GhoState {ghostState :: PlayerState, ghostMode :: GhostMode} deriving Eq

type Coords = (Int,Int)
type PlayerState = (Int, Coords, Double , Orientation, Int, Int)
--                 (ID,  (x,y), velocity, orientation, points, lives) 
data Mouth = Open | Closed deriving (Eq,Show)
data PacMode = Dying | Mega | Normal deriving (Eq,Show)
data GhostMode = Dead  | Alive deriving (Eq,Show)
data FoodType = Big | Little deriving (Eq)
data Color = Blue | Green | Purple | Red | Yellow | None deriving Eq 

data Play = Move Int Orientation deriving (Eq,Show)

type Instructions = [Instruction]
data Instruction = Instruct [(Int, Piece)] | Repeat Int deriving (Show, Eq)

instance Show State where
  show (State m ps p) = printMaze' mz ++ "Level: " ++ show p ++ "\nPlayers: \n" ++ (foldr (++) "\n" (map (\y-> printPlayerStats y) ps)) ++ "Estado do jogo: " ++ gameIsOver mz ps ++ "\n'r' para reiniciar o jogo.\n'q' para sair do jogo."
                        where mz = placePlayersOnMap ps m

instance Show PacState where
   show (PacState s o m Dying) = "X"
   show (PacState(a,b,c,R,i,l) _ Open m) = "{"
   show (PacState(a,b,c,R,i,l) _ Closed m) = "<"
   show (PacState(a,b,c,L,i,l) _ Open m) = "}"
   show (PacState(a,b,c,L,i,l) _ Closed m) = ">"
   show (PacState(a,b,c,U,i,l) _ Open m) = "V"
   show (PacState(a,b,c,U,i,l) _ Closed m) = "v"
   show (PacState(a,b,c,D,i,l) _ Open m) = "^"
   show (PacState(a,b,c,D,i,l) _ Closed m) = "|"
   show (PacState(a,b,c,Null,i,l) _ Closed m) = "<"
   show (PacState(a,b,c,Null,i,l) _ Open m) = "{"

instance Show Player where
   show (Pacman x) = show x
   show (Ghost x) = show x

instance Show GhoState where
   show (GhoState x Dead) = "?"
   show (GhoState x Alive) = "M"

instance Show FoodType where
   show (Big) = "o"
   show (Little) = "."

instance Show Piece where
   show (Wall) = "#"
   show (Empty) = " "
   show (Food z) = show z
   show (PacPlayer(Pacman(PacState(i,c,x,y,z,l) o m Normal))) = show(PacState(i,c,x,y,z,l) o m Normal)
   show (PacPlayer(Pacman(PacState(i,c,x,y,z,l) o m Mega))) = show(PacState(i,c,x,y,z,l) o m Mega)
   show (PacPlayer(Pacman(PacState(i,c,x,y,z,l) o m Dying))) = show(PacState(i,c,x,y,z,l) o m Dying)
   show (PacPlayer(Ghost z)) = show z

coloredString :: String -> Color -> String
coloredString x y
    | y == Blue ="\x1b[36m" ++  x ++ "\x1b[0m"
    | y == Red = "\x1b[31m" ++ x ++ "\x1b[0m"
    | y == Green = "\x1b[32m" ++ x ++ "\x1b[0m"
    | y == Purple ="\x1b[35m" ++ x ++ "\x1b[0m"
    | y == Yellow ="\x1b[33m" ++ x ++ "\x1b[0m"
    | otherwise =  "\x1b[0m" ++ x 

gameIsOver :: Maze -> [Player] -> String
gameIsOver maze players = if checkPacmanLife players then "PERDEU!" 
                          else if checkMaze maze then "A DECORRER..." else "GANHOU!"

checkMaze :: Maze -> Bool
checkMaze []    = False
checkMaze (h:t) = checkCorridor h || checkMaze t

checkCorridor :: Corridor -> Bool
checkCorridor [] = False
checkCorridor (h:t) = (h == Food Little || h == Food Big) || checkCorridor t

checkPacmanLife :: [Player] -> Bool
checkPacmanLife []                                        = False
checkPacmanLife ((Pacman(PacState(a,b,c,d,e,f) g h i)):t) = f == 0 || checkPacmanLife t
checkPacmanLife ((Ghost(GhoState(a,b,c,d,e,f) g)):t)      = checkPacmanLife t

getPlayerByID :: Int -> [Player] -> Player
getPlayerByID id (h:t) = if getPlayerID h == id then h else getPlayerByID id t

placePlayersOnMap :: [Player] -> Maze -> Maze
placePlayersOnMap [] x = x
placePlayersOnMap (x:xs) m = placePlayersOnMap xs ( replacePieceInMaze (getPlayerCoords x) (PacPlayer x) m )

printMaze' :: Maze -> String
printMaze' []  =  ""
printMaze' (x:xs) = foldr (++) "" ( map (\y -> show y) x )  ++ "\n" ++ printMaze' ( xs )

printPlayerStats :: Player -> String
printPlayerStats p = let (a,b,c,d,e,l) = getPlayerState p
                     in "ID:" ++ show a ++  " Points:" ++ show e ++ " Lives:" ++ show l ++"\n"

getPlayerID :: Player -> Int
getPlayerID (Pacman (PacState (x,_,_,_,_,_) _ _ _)) = x
getPlayerID (Ghost (GhoState (x,_,_,_,_,_) _)) = x

getPlayerPoints :: Player -> Int
getPlayerPoints (Pacman (PacState (_,_,_,_,h,_) _ _ _)) = h
getPlayerPoints (Ghost (GhoState (_,_,_,_,h,_) _)) = h

setPlayerCoords :: Player -> Coords -> Player
setPlayerCoords (Pacman (PacState (x,_,z,t,h,l) q c d)) (a,b) = Pacman (PacState (x,(a,b),z,t,h,l) q c d)
setPlayerCoords (Ghost (GhoState (x,_,z,t,h,l) q)) (a,b) = Ghost (GhoState (x,(a,b),z,t,h,l) q)

getPieceOrientation :: Piece -> Orientation
getPieceOrientation (PacPlayer p) =  getPlayerOrientation p
getPieceOrientation _ = Null

getPacmanMode :: Player -> PacMode
getPacmanMode (Pacman (PacState _ _ _ d)) = d

getPlayerState :: Player -> PlayerState
getPlayerState (Pacman (PacState a _ _ _)) = a
getPlayerState (Ghost (GhoState a _)) = a

getPlayerOrientation :: Player -> Orientation
getPlayerOrientation (Pacman (PacState (_,_,_,t,_,_) _ _ _)) = t
getPlayerOrientation (Ghost (GhoState (_,_,_,t,_,_) _)) = t

replacePieceInMaze :: Coords -> Piece -> Maze -> Maze
replacePieceInMaze (a,b) _ [] = []
replacePieceInMaze (a,b) p (x:xs) 
  | a == 0 = replacePiece b p x : xs 
  | otherwise = x : replacePieceInMaze (a-1,b) p xs

replacePiece :: Int -> a -> [a] -> [a]
replacePiece i _ [] = [] 
replacePiece i el (x:xs)
  |  i == 0 = el : xs 
  | otherwise =  x : replacePiece (i-1) el xs

getPlayerCoords :: Player -> Coords
getPlayerCoords (Pacman (PacState (_,y,_,_,_,_) _ _ _)) = y
getPlayerCoords (Ghost (GhoState (_,y,_,_,_,_) _)) = y