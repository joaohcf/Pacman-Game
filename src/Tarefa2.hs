{- |
= Introdução

Nesta tarefa, tinhamos como objetivo determinar o efeito de uma jogada no estado de jogo (dada uma descrição do estado do jogo e uma jogada de um dos jogadores).

= Metas

Tal como mencionado na introdução, a meta desta tarefa era determinar o efeito de uma jogada no estado do jogo (sabendo que esta tarefa apenas exigia a implementação de realização de jogadas para jogadores do tipo Pacman).
Cada jogada consiste na movimentação de um jogador identificado com o seu único ID (para todas as direções possíveis, isto é, cima, baixo, direita, esquerda) e atualizar
o efeito dessa jogada no labirinto atual (função objetivo da tarefa 'play'). Para tal, tivemos de criar as funções que de certo modo, traduziam as reações do jogadores e/ou
do labirinto face a todas as possíveis situações, criando deste modo todos os possíveis resultados.

= Conclusão

Concluindo este resumo da tarefa 2, afirmamos que, esta foi a tarefa mais trabalhosa da primeira parte do trabalho prático, que no entanto, apesar de várias alterações no código,
de forma a que a tarefa fosse completa corretamente e, eventualmente, de maneira a que a qualidade do código fosse melhorada, pensamos que o nosso trabalho foi bastante satisfatório, passando
em todos os testes no sistema de avaliação dos professores.
-}
module Tarefa2 where

import Types

-- = Função Principal
-- | Faz uma jogada sobre um determinado jogador e devolve um novo estado do jogo.
--
play :: Play -> State -> State
play (Move id orientation) (State maze playersState level) = State newMaze newPlayersState level
                                                            where
                                                                newPlayersState = if checkPacmanLife playersState || checkMaze newMaze == False then freezePlayers playersState else checkPlayers maze $ gameWithMega (playerMaze (Move id orientation) playersState maze)
                                                                newMaze         = updateMaze (getPlayerByID id playersState) maze

-- | Congela todos os jogadores.
--
freezePlayers :: [Player] -> [Player]
freezePlayers []                                         = []
freezePlayers ((Pacman(PacState (a,b,c,d,e,f) g h i)):t) = Pacman(PacState (a,b,c,Null,e,f) g h i) : freezePlayers t
freezePlayers ((Ghost(GhoState (a,b,c,d,e,f) g)):t)      = Ghost(GhoState (a,b,c,Null,e,f) g) : freezePlayers t

-- | Altera o labirinto.
--
updateMaze :: Player -> Maze -> Maze
updateMaze pac@(Pacman (PacState a b c Normal)) maze = replacePieceInMaze (getPlayerCoords pac) Empty maze
updateMaze pac@(Pacman (PacState a b c Mega)) maze   = replacePieceInMaze (getPlayerCoords pac) Empty maze
updateMaze pac@(Pacman (PacState a b c Dying)) maze  = maze
updateMaze ghost@(Ghost _) maze                      = maze

-- | Consoante o estado do jogo onde haja ou não algum jogador Pacman em modo Mega, altera o estado dos fantasmas.
--
gameWithMega :: [Player] -> [Player]
gameWithMega (h:t) | verifyNewMega (h:t) = turnGhostsDead (h:t)
                   | verifyNoMega (h:t)  = turnGhostsAlive (h:t)
                   | otherwise           = h:t

-- | Compara um jogador com vários e altera o seu estado caso seja ncessário.
--
checkPlayers :: Maze -> [Player] -> [Player]
checkPlayers maze [] = []
checkPlayers maze (h:t) = if getPlayerID h < getPlayerID (head t) then
                              x : checkPlayers  maze (t ++ [x])
                          else
                              [x]
                          where x = playersWithSameCoords maze h t

-- | Verifica se tem jogadores na mesma coordenada e devolve o jogador com estado alterado.
--
playersWithSameCoords :: Maze -> Player -> [Player] -> Player
playersWithSameCoords maze x []    = x
playersWithSameCoords maze x (h:t) = if getPlayerCoords x == getPlayerCoords h then
                                        playersWithSameCoords maze (actionPlayerPlayer maze x h) t
                                    else
                                        playersWithSameCoords maze x t

-- | Muda o estado de um jogador consoante o jogador em que embate.
--
actionPlayerPlayer :: Maze -> Player -> Player -> Player
actionPlayerPlayer maze (Pacman a) (Pacman b)                                                   = Pacman a
actionPlayerPlayer maze (Ghost a) (Ghost b)                                                     = Ghost a
actionPlayerPlayer maze (Pacman (PacState (a,b,c,d,e,f) g h Normal)) (Ghost (GhoState x Alive)) = if f == 0 then Pacman (PacState (a,b,c,d,e,0) g h Dying) else Pacman (PacState (a,(1,1),c,d,e,f-1) 0 h Normal)
actionPlayerPlayer maze (Pacman (PacState (a,b,c,d,e,f) g h Normal)) (Ghost (GhoState x Dead))  = Pacman (PacState (a,b,c,d,e+10,f) g h Normal)
actionPlayerPlayer maze (Pacman (PacState (a,b,c,d,e,f) g h Mega)) (Ghost (GhoState x Alive))   = if f == 0 then Pacman (PacState (a,b,c,d,e,0) g h Dying) else Pacman (PacState (a,(1,1),c,d,e,f-1) 0 h Normal)
actionPlayerPlayer maze (Pacman (PacState (a,b,c,d,e,f) g h Mega)) (Ghost (GhoState x Dead))    = Pacman (PacState (a,b,c,d,e+10,f) g h Mega)
actionPlayerPlayer maze (Pacman (PacState a b c Dying)) (Ghost (GhoState x y))                  = Pacman (PacState a b c Dying)
actionPlayerPlayer maze (Ghost (GhoState (x,y,z,w,r,t) Alive)) (Pacman (PacState a b c Normal)) = Ghost (GhoState (x,y,z,w,r,t) Alive)
actionPlayerPlayer maze (Ghost (GhoState (x,y,z,w,r,t) Dead)) (Pacman (PacState a b c Normal))  = teleportToHouse maze $ Ghost (GhoState (x,y,z*2,w,r,t) Alive)
actionPlayerPlayer maze (Ghost (GhoState (x,y,z,w,r,t) Alive)) (Pacman (PacState a b c Mega))   = Ghost (GhoState (x,y,z,w,r,t) Alive)
actionPlayerPlayer maze (Ghost (GhoState (x,y,z,w,r,t) Dead)) (Pacman (PacState a b c Mega))    = teleportToHouse maze $ Ghost (GhoState (x,y,z*2,w,r,t) Alive)
actionPlayerPlayer maze (Ghost (GhoState x y)) (Pacman (PacState a b c Dying))                  = Ghost (GhoState x y)

-- | Teletransporta um fantasma para a casa de fantasmas.
--
teleportToHouse :: Maze -> Player -> Player
teleportToHouse maze player = setPlayerCoords player (middleHeight maze-1,middleWidth maze)

-- | Verifica se o jogador vai entrar na casa.
--
joinHouse :: Maze -> Coords -> Coords -> Bool
joinHouse maze (x,y) (a,b) = if x >= middleHeight maze-2 && a == middleHeight maze-2 && y >= middleWidth maze-1 && y <= middleWidth maze+1 then 
                                False 
                             else
                                if x < middleHeight maze-2 && a == middleHeight maze-2 && y >= middleWidth maze-1 && y <= middleWidth maze+1 then
                                    True 
                                else
                                    False

-- | Relação jogador & labirinto.
--
playerMaze :: Play -> [Player] -> Maze -> [Player]
playerMaze (Move id orientation) (h:t) maze =  if getPlayerID h == id then
                                                    if getPieceOrientation (PacPlayer h) == orientation then
                                                        if snd playerNewCoords >= 0 && snd playerNewCoords <= corridorLength then
                                                            if piece == Wall || house then
                                                                h : t
                                                            else
                                                                actionPlayerPiece playerWithNewCoords piece : t
                                                        else
                                                            tunnelTeleport playerWithNewCoords corridorLength : t
                                                    else
                                                        setPlayerOrientation h orientation : t
                                                else
                                                    h : playerMaze (Move id orientation) t maze
                                                where   piece               = checkNextPiece playerNewCoords maze
                                                        house               = joinHouse maze (getPlayerCoords h) playerNewCoords
                                                        playerNewCoords     = getPlayerCoords playerWithNewCoords
                                                        playerWithNewCoords = setPlayerCoords h (getNewCoords (getPlayerCoords h) orientation)
                                                        corridorLength      = mazeWidth maze

-- | Procura no labirinto a peça na posição para a qual um jogador transita.
--
checkNextPiece :: Coords -> Maze -> Piece
checkNextPiece (x,y) (h:t) = if x == 0 then checkPiece y h else checkNextPiece (x-1,y) t

-- | Devolve a peça que se encontra na posição para a qual um jogador transita.
--
checkPiece :: Int -> Corridor -> Piece
checkPiece i (h:t) = if i == 0 then h else checkPiece (i-1) t

-- | Muda o estado de um jogador consoante a peça em que o jogador embate.
--
actionPlayerPiece :: Player -> Piece -> Player
actionPlayerPiece (Pacman(PacState (x,y,z,t,h,l) b c Normal)) Empty         = Pacman (PacState (x,y,z,t,h,l) b c Normal)
actionPlayerPiece (Pacman(PacState (x,y,z,t,h,l) b c Normal)) (Food Little) = Pacman (PacState (x,y,z,t,h+1,l) b c Normal)
actionPlayerPiece (Pacman(PacState (x,y,z,t,h,l) b c Normal)) (Food Big)    = Pacman (PacState (x,y,z,t,h+5,l) pacmanDefaultMegaTime c Mega)
actionPlayerPiece (Pacman(PacState (x,y,z,t,h,l) b c Mega)) Empty           = Pacman (PacState (x,y,z,t,h,l) b c Mega)
actionPlayerPiece (Pacman(PacState (x,y,z,t,h,l) b c Mega)) (Food Little)   = Pacman (PacState (x,y,z,t,h+1,l) b c Mega)
actionPlayerPiece (Pacman(PacState (x,y,z,t,h,l) b c Mega)) (Food Big)      = Pacman (PacState (x,y,z,t,h+5,l) pacmanDefaultMegaTime c Mega)
actionPlayerPiece (Pacman(PacState a b c d)) _                              = Pacman (PacState a b c d)
actionPlayerPiece (Ghost (GhoState a b)) _                                  = Ghost (GhoState a b)

-- | Tempo em modo Mega do Pacman.
--
pacmanDefaultMegaTime :: Double
pacmanDefaultMegaTime = 10

-- | Define as coordenadas de um jogador.
--
setPlayerOrientation :: Player -> Orientation -> Player
setPlayerOrientation (Pacman (PacState (a,b,c,_,e,f) g h i)) x = Pacman (PacState (a,b,c,x,e,f) g h i)
setPlayerOrientation (Ghost (GhoState (a,b,c,_,e,f) g)) x      = Ghost (GhoState (a,b,c,x,e,f) g)

-- | Devolve as coordenadas de um determinado jogador.
--
getPlayerCoordsByID :: Int -> [Player] -> Coords
getPlayerCoordsByID _ [] = (0,0)
getPlayerCoordsByID id (h:t) = if id == getPlayerID h then getPlayerCoords h else getPlayerCoordsByID id t

-- | Verifica se um jogador Pacman entrou em modo Mega.
--
verifyNewMega :: [Player] -> Bool
verifyNewMega []                             = False
verifyNewMega ((Ghost x):t)                  = verifyNewMega t
verifyNewMega ((Pacman(PacState a b c d)):t) = d == Mega && b == pacmanDefaultMegaTime || verifyNewMega t

-- | Verifica se não existe um jogador Pacman em modo Mega.
verifyNoMega :: [Player] -> Bool
verifyNoMega []                             = False
verifyNoMega ((Ghost x):t)                  = verifyNoMega t
verifyNoMega ((Pacman(PacState a b c d)):t) = d == Normal || verifyNoMega t

-- | Muda o estado dos fantasmas para Alive.
--
turnGhostsAlive :: [Player] -> [Player]
turnGhostsAlive []                                        = []
turnGhostsAlive ((Pacman(PacState a b c d)):t)            = Pacman(PacState a b c d) : turnGhostsAlive t
turnGhostsAlive ((Ghost(GhoState (a,b,c,d,e,f) Alive)):t) = Ghost(GhoState(a,b,c,d,e,f) Alive) : turnGhostsAlive t
turnGhostsAlive ((Ghost(GhoState (a,b,c,d,e,f) Dead)):t)  = Ghost(GhoState(a,b,c*2,d,e,f) Alive) : turnGhostsAlive t

-- | Muda o estado dos fantasmas para Dead.
--
turnGhostsDead :: [Player] -> [Player]
turnGhostsDead []                                        = []
turnGhostsDead ((Pacman(PacState a b c d)):t)            = Pacman(PacState a b c d) : turnGhostsDead t
turnGhostsDead ((Ghost(GhoState (a,b,c,d,e,f) Alive)):t) = Ghost(GhoState(a,b,c/2,invertOrientation d,e,f) Dead) : turnGhostsDead t
turnGhostsDead ((Ghost(GhoState (a,b,c,d,e,f) Dead)):t)  = Ghost(GhoState(a,b,c,d,e,f) Dead) : turnGhostsDead t

-- | Inverte a oritenção.
--
invertOrientation :: Orientation -> Orientation
invertOrientation U = D
invertOrientation D = U
invertOrientation R = L
invertOrientation L = R
invertOrientation Null = Null 

-- | Devolve as novas coordenadas de um jogador consoante a sua orientação.
--
getNewCoords :: Coords -> Orientation -> Coords
getNewCoords (x,y) R = (x,y+1)
getNewCoords (x,y) L = (x,y-1)
getNewCoords (x,y) U = (x-1,y)
getNewCoords (x,y) D = (x+1,y)
getNewCoords (x,y) Null = (x,y)

-- | Teletransporta um jogador que entrou no túnel.
--
tunnelTeleport :: Player -> Int -> Player
tunnelTeleport (Pacman(PacState(a,(x,y),b,L,d,e) f g h)) length = if y < 0 then Pacman(PacState(a,(x,length),b,L,d,e) f g h) else Pacman(PacState(a,(x,y),b,L,d,e) f g h)
tunnelTeleport (Pacman(PacState(a,(x,y),b,R,d,e) f g h)) length = if y > length then Pacman(PacState(a,(x,0),b,R,d,e) f g h) else Pacman(PacState(a,(x,y),b,R,d,e) f g h)
tunnelTeleport (Pacman(PacState(a,(x,y),b,c,d,e) f g h)) _      = Pacman(PacState(a,(x,y),b,c,d,e) f g h)
tunnelTeleport (Ghost(GhoState(a,(x,y),b,L,d,e) f)) length      = if y < 0 then Ghost(GhoState(a,(x,length),b,L,d,e) f) else Ghost(GhoState(a,(x,y),b,L,d,e) f)
tunnelTeleport (Ghost(GhoState(a,(x,y),b,R,d,e) f)) length      = if y > length then Ghost(GhoState(a,(x,0),b,R,d,e) f) else Ghost(GhoState(a,(x,y),b,R,d,e) f)
tunnelTeleport (Ghost(GhoState(a,(x,y),b,c,d,e) f)) _           = Ghost(GhoState(a,(x,y),b,c,d,e) f)

-- == Funções auxiliares
-- | Calcula metade da altura do labirinto
--
middleHeight :: Maze -> Int
middleHeight maze = div (length maze) 2

-- | Calcula metade da largura do labirinto
--
middleWidth :: Maze -> Int
middleWidth (h:_) = div (length h) 2

-- | Calcula a largura total do labirinto
--
mazeWidth :: Maze -> Int
mazeWidth (h:_) = length h-1